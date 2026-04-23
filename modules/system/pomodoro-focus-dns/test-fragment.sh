#!/usr/bin/env bash
# Pure bash checks: parsing rules mirrored from pomodoro-dns-apply.sh.
set -euo pipefail

valid_label() {
  local h="$1"
  if ((${#h} > 253)); then return 1; fi
  if [[ ! "$h" =~ ^[a-zA-Z0-9]([a-zA-Z0-9.-]*[a-zA-Z0-9])?$ ]]; then return 1; fi
  if [[ "$h" == *..* ]]; then return 1; fi
  return 0
}

emit_for_line() {
  local line="$1"
  local rest host

  if [[ "$line" =~ ^[0-9]{1,3}(\.[0-9]{1,3}){3}[[:space:]]+(.+)$ ]]; then
    rest="${BASH_REMATCH[2]}"
    rest="${rest%%#*}"
    rest="${rest#"${rest%%[![:space:]]*}"}"
    rest="${rest%"${rest##*[![:space:]]}"}"
    [[ -z "$rest" ]] && return 0
    for host in $rest; do
      if valid_label "$host"; then
        printf 'address=/%s/0.0.0.0\n' "$host"
      fi
    done
  elif valid_label "$line"; then
    printf 'address=/%s/0.0.0.0\n' "$line"
  fi
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
  while IFS= read -r addrline; do
    [[ -z "$addrline" ]] && continue
    printf '%s\n' "$addrline" >>"$out"
  done < <(emit_for_line "$line")
done <"$tmp"

expected="$(mktemp)"
printf '%s\n' 'address=/good.example/0.0.0.0' 'address=/ok.test/0.0.0.0' >"$expected"
cmp -s "$out" "$expected" || fail "bare-hostname case mismatch"

rm -f "$out" "$expected"

# /etc/hosts-style lines: extract hostnames after IPv4.
printf '%s\n' '127.0.0.1 scotusblog.com' '0.0.0.0 a.example b.example' >"$tmp"
out="$(mktemp)"
while IFS= read -r line || [[ -n "$line" ]]; do
  line="${line#"${line%%[![:space:]]*}"}"
  line="${line%"${line##*[![:space:]]}"}"
  [[ -z "$line" ]] && continue
  while IFS= read -r addrline; do
    [[ -z "$addrline" ]] && continue
    printf '%s\n' "$addrline" >>"$out"
  done < <(emit_for_line "$line")
done <"$tmp"

expected="$(mktemp)"
printf '%s\n' \
  'address=/scotusblog.com/0.0.0.0' \
  'address=/a.example/0.0.0.0' \
  'address=/b.example/0.0.0.0' >"$expected"
cmp -s "$out" "$expected" || fail "hosts-style case mismatch"

rm -f "$out" "$expected"
echo "OK: test-fragment.sh"
