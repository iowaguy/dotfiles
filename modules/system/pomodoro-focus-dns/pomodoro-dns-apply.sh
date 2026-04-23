#!/usr/bin/env bash
# Rewrites the dnsmasq focus fragment and restarts dnsmasq.
#
# Important: `systemctl reload` sends SIGHUP; dnsmasq only reapplies that for
# resolv-file / hosts, *not* for conf-file includes like pomodoro-focus.conf.
# Without a restart, new address= lines never take effect.
#
# Intended to run as root (via sudo). User `ben` is granted NOPASSWD for the
# nix-store path of this script only. Each hostname from the blocklist is
# emitted as an address= sinkhole line after validation.
#
# Blocklist lines may be either:
#   - A bare hostname:   nytimes.com
#   - /etc/hosts style:  127.0.0.1 scotusblog.com
#     (IP is ignored; only the hostname tokens are sinkholed.)
set -euo pipefail

FRAGMENT=/var/lib/dnsmasq/pomodoro-focus.conf
BLOCKLIST='@blocklistPath@'
MAX_LINES=512

valid_label() {
  local h="$1"
  if ((${#h} > 253)); then
    return 1
  fi
  if [[ ! "$h" =~ ^[a-zA-Z0-9]([a-zA-Z0-9.-]*[a-zA-Z0-9])?$ ]]; then
    return 1
  fi
  if [[ "$h" == *..* ]]; then
    return 1
  fi
  return 0
}

# Append sinkhole lines for one logical blocklist row; honors /etc/hosts layout.
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

tmp="$(mktemp)"
cleanup() { rm -f "$tmp"; }
trap cleanup EXIT

case "${1:-}" in
  on)
    lines=0
    if [[ -f "$BLOCKLIST" ]]; then
      while IFS= read -r line || [[ -n "$line" ]]; do
        [[ "$line" =~ ^[[:space:]]*# ]] && continue
        line="${line#"${line%%[![:space:]]*}"}"
        line="${line%"${line##*[![:space:]]}"}"
        [[ -z "$line" ]] && continue
        while IFS= read -r addrline; do
          [[ -z "$addrline" ]] && continue
          printf '%s\n' "$addrline" >>"$tmp"
          lines=$((lines + 1))
          if ((lines > MAX_LINES)); then
            echo "pomodoro-dns-apply: more than $MAX_LINES valid entries" >&2
            exit 1
          fi
        done < <(emit_for_line "$line")
      done <"$BLOCKLIST"
    fi
    ;;
  off)
    : >"$tmp"
    ;;
  *)
    echo "usage: pomodoro-dns-apply on|off" >&2
    exit 2
    ;;
esac

install -m 0644 "$tmp" "$FRAGMENT"
systemctl restart dnsmasq.service
