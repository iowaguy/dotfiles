#!/usr/bin/env bash
# Pure bash check: same hostname rules as pomodoro-dns-apply (no root/systemd).
set -euo pipefail

valid_label() {
  local h="$1"
  if ((${#h} > 253)); then return 1; fi
  if [[ ! "$h" =~ ^[a-zA-Z0-9]([a-zA-Z0-9.-]*[a-zA-Z0-9])?$ ]]; then return 1; fi
  if [[ "$h" == *..* ]]; then return 1; fi
  return 0
}

fail() { echo "FAIL: $*" >&2; exit 1; }

valid_label example.com || fail "example.com should be valid"
valid_label a || fail "a should be valid"
valid_label bad..name && fail "double-dot should be invalid"
valid_label "-x" && fail "leading hyphen invalid"
valid_label "" && fail "empty invalid"

tmp="$(mktemp)"
cleanup() { rm -f "$tmp"; }
trap cleanup EXIT

printf '%s\n' '# comment' '' 'good.example' 'bad..z' 'ok.test' >"$tmp"

out="$(mktemp)"
while IFS= read -r line || [[ -n "$line" ]]; do
  [[ "$line" =~ ^[[:space:]]*# ]] && continue
  line="${line#"${line%%[![:space:]]*}"}"
  line="${line%"${line##*[![:space:]]}"}"
  [[ -z "$line" ]] && continue
  valid_label "$line" && printf 'address=/%s/0.0.0.0\n' "$line" >>"$out"
done <"$tmp"

expected="$(mktemp)"
printf '%s\n' 'address=/good.example/0.0.0.0' 'address=/ok.test/0.0.0.0' >"$expected"
cmp -s "$out" "$expected" || fail "fragment mismatch (see $out vs $expected)"

rm -f "$out" "$expected"
echo "OK: test-fragment.sh"
