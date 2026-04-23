# Pomodoro focus DNS blocking — implementation plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** When the Pomodoro **work** phase is active, dnsmasq sinkholes hostnames from `~/.config/pomodoro/blocklist.txt`; during **breaks** and when the timer **stops**, that layer is removed; existing `networking.extraHosts` behavior stays as-is.

**Architecture:** NixOS runs **dnsmasq** as the system stub resolver (`resolveLocalQueries`), forwarding to **1.1.1.1** and **9.9.9.9**. A static second `conf-file` points at `/var/lib/dnsmasq/pomodoro-focus.conf`, rewritten by a **root helper** `pomodoro-dns-apply` (`on` / `off`). User **ben** may run that helper via **sudo NOPASSWD** for that path only. **home-manager** ships the blocklist file template; **pomodoro** shell scripts call the helper behind `command -v` so machines without the module are unaffected.

**Tech stack:** NixOS 25.11 (`services.dnsmasq.settings`), systemd, bash, sudo, home-manager, existing `modules/home/pomodoro` scripts.

---

## File map

| File | Role |
|------|------|
| `docs/superpowers/specs/2026-04-23-pomodoro-focus-dns-blocking-design.md` | Approved spec (read-only reference) |
| `modules/system/pomodoro-focus-dns/default.nix` | **Create** — options, dnsmasq, helper package, sudoers, tmpfiles for fragment |
| `modules/system/pomodoro-focus-dns/pomodoro-dns-apply.sh` | **Create** — root helper source (patched into store via `substituteAll` or `writeShellApplication`) |
| `flake.nix` | **Modify** — import `pomodoro-focus-dns` in `nixosConfigurations.x1-2021` only |
| `machines/x1-2021/machine.nix` | **Modify** — `services.pomodoroFocusDns.enable = true;` (host-specific services already live here) |
| `modules/home/pomodoro/default.nix` | **Modify** — ensure blocklist placeholder under XDG config (optional but recommended) |
| `modules/home/pomodoro/pomodoro.sh` | **Modify** — `on` at work start, `off` after work / in trap |
| `modules/home/pomodoro/pomodoro-control.sh` | **Modify** — `off` when killing pomodoro |
| `modules/system/pomodoro-focus-dns/test-fragment.sh` | **Create** — pure bash test (no root) for label validation + line generation |

**Note:** The flake imports `./machines/x1-2021` (see `default.nix` → `machine.nix`). Add the enable flag in `machine.nix` beside other `services` options.

---

### Task 1: NixOS module skeleton and dnsmasq wiring

**Files:**
- Create: `modules/system/pomodoro-focus-dns/default.nix`
- Create: `modules/system/pomodoro-focus-dns/pomodoro-dns-apply.sh` (minimal stub: only `off` + reload, `on` echoes “not implemented” — **or** implement fully in Task 2; if stub, Task 2 replaces body)
- Modify: `flake.nix` (add `./modules/system/pomodoro-focus-dns` to x1-2021 `modules` list)
- Modify: `machines/x1-2021/machine.nix` — set `services.pomodoroFocusDns.enable = true;`

**Module interface (paste into `default.nix` and adjust imports):**

```nix
# modules/system/pomodoro-focus-dns/default.nix
{ config, lib, pkgs, ... }:

let
  cfg = config.services.pomodoroFocusDns;
  focusConf = "/var/lib/dnsmasq/pomodoro-focus.conf";
  helper = pkgs.writeShellApplication {
    name = "pomodoro-dns-apply";
    runtimeInputs = with pkgs; [ coreutils systemd ];
    text = builtins.readFile ./pomodoro-dns-apply.sh;
  };
in
{
  options.services.pomodoroFocusDns = {
    enable = lib.mkEnableOption "Pomodoro work-phase extra DNS blocking via dnsmasq";
    blocklistPath = lib.mkOption {
      type = lib.types.str;
      default = "/home/ben/.config/pomodoro/blocklist.txt";
      description = "Absolute path to the user-editable hostname list.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.dnsmasq = {
      enable = true;
      resolveLocalQueries = true;
      settings = {
        server = [ "1.1.1.1" "9.9.9.9" ];
        conf-file = [ focusConf ];
      };
    };

    environment.systemPackages = [ helper ];

    # Ensure dnsmasq’s conf-file exists before the daemon parses config (empty = no extra sinkholes).
    systemd.tmpfiles.rules = [
      "f /var/lib/dnsmasq/pomodoro-focus.conf 0644 root root -"
    ];

    security.sudo.extraRules = [
      {
        users = [ "ben" ];
        commands = [
          {
            command = "${helper}/bin/pomodoro-dns-apply";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];
  };
}
```

**Stub `pomodoro-dns-apply.sh` (replace in Task 2 with full logic):**

```bash
#!/usr/bin/env bash
set -euo pipefail
FRAGMENT=/var/lib/dnsmasq/pomodoro-focus.conf
case "${1:-}" in
  off)
    : >"$FRAGMENT"
    ;;
  *)
    echo "stub" >&2; exit 1 ;;
esac
systemctl reload dnsmasq.service
```

- [ ] **Step 1:** Add the three files and wire `flake.nix` + x1-2021 enable flag.

- [ ] **Step 2:** Evaluate config (no full switch yet)

Run:

```bash
cd /home/ben/areas/system-management/dotfiles
nix eval .#nixosConfigurations.x1-2021.config.services.dnsmasq.enable --json
```

Expected stdout: `true`

- [ ] **Step 3:** Commit

```bash
git add modules/system/pomodoro-focus-dns flake.nix machines/x1-2021/machine.nix
git commit -m "feat(nixos): scaffold pomodoro-focus-dns + dnsmasq"
```

---

### Task 2: Full `pomodoro-dns-apply` helper

**Files:**
- Modify: `modules/system/pomodoro-focus-dns/default.nix` — replace the Task 1 `writeShellApplication` helper with a small derivation: `substituteAll` on `pomodoro-dns-apply.sh` for `@blocklistPath@`, install into `$out/bin`, then `wrapProgram` so `install`, `mktemp`, `systemctl`, and `bash` resolve on a minimal PATH.

- Modify: `modules/system/pomodoro-focus-dns/pomodoro-dns-apply.sh` — full implementation below.

**`pomodoro-dns-apply.sh` (use `@blocklistPath@` if using substituteAll):**

```bash
#!/usr/bin/env bash
# Rewrites the dnsmasq focus fragment and reloads dnsmasq.
# Root-only; invoked via sudo with a fixed path. Validates each label
# before emitting address= lines to avoid oversized or malicious input.
set -euo pipefail

FRAGMENT=/var/lib/dnsmasq/pomodoro-focus.conf
BLOCKLIST='@blocklistPath@'
MAX_LINES=512

valid_label() {
  local h="$1"
  # Length bound: single DNS label max 63; full hostname often <=253 — we keep a practical cap via line length.
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
        if valid_label "$line"; then
          printf 'address=/%s/0.0.0.0\n' "$line" >>"$tmp"
          lines=$((lines + 1))
        fi
        if ((lines > MAX_LINES)); then
          echo "pomodoro-dns-apply: more than $MAX_LINES valid entries" >&2
          exit 1
        fi
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
systemctl reload dnsmasq.service
```

**Nix snippet to substitute and wrap** (replace `helper` definition from Task 1):

```nix
  substituted = pkgs.substituteAll {
    src = ./pomodoro-dns-apply.sh;
    blocklistPath = cfg.blocklistPath;
  };
  helper = pkgs.stdenvNoCC.mkDerivation {
    name = "pomodoro-dns-apply";
    nativeBuildInputs = [ pkgs.makeWrapper ];
    dontUnpack = true;
    installPhase = ''
      install -Dm755 ${substituted} $out/bin/pomodoro-dns-apply
      wrapProgram $out/bin/pomodoro-dns-apply \
        --prefix PATH : ${lib.makeBinPath (with pkgs; [ bash coreutils systemd ])}
    '';
  };
```

(`substituteAll` yields a store path to the substituted script; interpolating it in `installPhase` avoids unpack quirks.)

- [ ] **Step 1:** Implement substitution + full script; remove stub.

- [ ] **Step 2:** `nix eval` again as in Task 1.

- [ ] **Step 3:** Commit

```bash
git add modules/system/pomodoro-focus-dns/
git commit -m "feat(nixos): pomodoro-dns-apply on/off with validation"
```

---

### Task 3: Automated fragment test (no root, no systemd)

**Files:**
- Create: `modules/system/pomodoro-focus-dns/test-fragment.sh`

**Content:**

```bash
#!/usr/bin/env bash
# Validates hostname filtering logic matches expectations without touching /var/lib or systemd.
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

want=$'address=/good.example/0.0.0.0\naddress=/ok.test/0.0.0.0\n'
got="$(cat "$out")"
[[ "$got" == "$want" ]] || fail "want:\n$want\ngot:\n$got"

echo "OK: test-fragment.sh"
```

- [ ] **Step 1:** Add file.

- [ ] **Step 2:** Run test

```bash
bash modules/system/pomodoro-focus-dns/test-fragment.sh
```

Expected stdout: `OK: test-fragment.sh`  
Expected exit code: `0`

- [ ] **Step 3:** Commit

```bash
git add modules/system/pomodoro-focus-dns/test-fragment.sh
git commit -m "test: pomodoro-dns fragment generation logic"
```

---

### Task 4: home-manager blocklist placeholder

**Files:**
- Modify: `modules/home/pomodoro/default.nix`

**Addition** (inside `home.file` or prefer `xdg.configFile`):

```nix
  xdg.configFile."pomodoro/blocklist.txt" = {
    force = true;
    text = ''
      # One hostname per line during Pomodoro *work* phases (dnsmasq sinkhole).
      # example.com
    '';
  };
```

- [ ] **Step 1:** Add the file; run `home-manager` build or full `nixos-rebuild` when convenient.

- [ ] **Step 2:** Commit

```bash
git add modules/home/pomodoro/default.nix
git commit -m "feat(home): default Pomodoro DNS blocklist file"
```

---

### Task 5: Pomodoro shell hooks

**Files:**
- Modify: `modules/home/pomodoro/pomodoro.sh`
- Modify: `modules/home/pomodoro/pomodoro-control.sh`

**Add to both files near the top (after `bin_dir` / exports), in `pomodoro.sh`:**

```bash
# Toggle optional system DNS focus layer (dnsmasq). No-op if the helper is not installed.
pomodoro_dns_apply() {
  local mode="$1"
  if command -v pomodoro-dns-apply >/dev/null 2>&1; then
    if ! sudo -n pomodoro-dns-apply "$mode" 2>/tmp/pomodoro-dns-apply.err; then
      dunstify --urgency=low "Pomodoro DNS ($mode) failed — see /tmp/pomodoro-dns-apply.err"
    fi
  fi
}
```

**In `pomodoro.sh` `ctrl_c`:** before `printf ... status_file`, add `pomodoro_dns_apply off`.

**In `pomodoro.sh` main loop:**  
- After `dunstify ... "Work session started!"` (and after `echo "Work session started!"`), add `pomodoro_dns_apply on`.  
- After `countdown $work_duration "Work"` returns (before break dunstify), add `pomodoro_dns_apply off`.

**In `pomodoro-control.sh`:** in the `else` branch (killing pomodoro), after `kill $PID` (or before — either is fine; prefer **after** kill so the child cannot race), add:

```bash
if command -v pomodoro-dns-apply >/dev/null 2>&1; then
  sudo -n pomodoro-dns-apply off 2>/tmp/pomodoro-dns-apply.err || true
fi
```

(Use `|| true` here so a kill-path still resets status even if DNS reload fails; optional dunst could duplicate Task 5 helper — YAGNI on double notify.)

- [ ] **Step 1:** Edit both scripts with extensive comments per repo convention.

- [ ] **Step 2:** Shell syntax check

```bash
bash -n /home/ben/areas/system-management/dotfiles/modules/home/pomodoro/pomodoro.sh
bash -n /home/ben/areas/system-management/dotfiles/modules/home/pomodoro/pomodoro-control.sh
```

Expected: no output, exit `0`.

- [ ] **Step 3:** Commit

```bash
git add modules/home/pomodoro/pomodoro.sh modules/home/pomodoro/pomodoro-control.sh
git commit -m "feat(pomodoro): toggle focus DNS for work phases"
```

---

### Task 6: Host integration verification (manual, on x1-2021)

**Files:** none (runtime)

- [ ] **Step 1:** `sudo nixos-rebuild switch` on x1-2021 (from flake).

- [ ] **Step 2:** Confirm resolver points at local stub

Run:

```bash
resolvectl status | head -40
```

Expected: mentions `127.0.0.1` or `DNS Servers: 127.0.0.1` for the active interface (exact wording varies).

- [ ] **Step 3:** Baseline DNS

```bash
dig +short A example.com @127.0.0.1 | head -1
```

Expected: a non-empty A record (IPv4 address).

- [ ] **Step 4:** Add a **test** hostname only in `~/.config/pomodoro/blocklist.txt` (e.g. `never-resolve.invalid.test` — use a name you do not rely on, or a real domain you own for testing).

- [ ] **Step 5:** Run `sudo pomodoro-dns-apply on` then:

```bash
dig +short A never-resolve.invalid.test @127.0.0.1
```

Expected: `0.0.0.0` (or dnsmasq’s configured sink response).

- [ ] **Step 6:** `sudo pomodoro-dns-apply off` — same `dig` should **not** return `0.0.0.0` (may be NXDOMAIN or real record).

- [ ] **Step 7:** Exercise xmobar Pomodoro: start work → confirm `on`; wait for work end → confirm break unblocks; stop from xmobar → confirm `off`.

- [ ] **Step 8:** Document any firewall or VPN quirks in a short comment in `modules/system/pomodoro-focus-dns/default.nix` only if discovered.

- [ ] **Step 9:** Commit only if you added comments or fixes from Step 8.

---

## Spec coverage (self-review)

| Spec requirement | Task(s) |
|------------------|---------|
| Work-only blocking | Task 5 (on after work start, off after work countdown) |
| Off on break / stop / SIGINT | Task 5 + control script |
| DNS-level via system resolver | Tasks 1–2 (dnsmasq + fragment) |
| User blocklist under `~/.config/pomodoro/` | Task 4 |
| Coexist with `extraHosts` | Task 1 (no change to `hosts.txt`); nsswitch still prefers files |
| Validated input, sudo scoped | Task 2 + Task 1 sudoers |
| Reload failure surfaced | Task 5 (`dunstify`, stderr file) |
| Manual testing | Task 6 |

**Placeholder scan:** None intentional; `never-resolve.invalid.test` is an example test name — replace with a safe choice on your network.

**Type consistency:** `pomodoro-dns-apply` arity is always one argument `on|off`; Nix `blocklistPath` matches `BLOCKLIST` substitution.

---

**Plan complete and saved to `docs/superpowers/plans/2026-04-23-pomodoro-focus-dns-blocking.md`. Two execution options:**

1. **Subagent-driven (recommended)** — dispatch a fresh subagent per task, review between tasks, fast iteration  
2. **Inline execution** — run tasks in this session using executing-plans, batch execution with checkpoints  

**Which approach do you want?**
