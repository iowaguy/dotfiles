# Design: Pomodoro work-phase DNS blocking

## Summary

Extend the existing xmobar-triggered Pomodoro workflow so that **during work phases only**, the system applies **additional DNS-level blocks** for hostnames listed in a **user-editable file** under the home directory (`~/.config/pomodoro/blocklist.txt` or the path chosen at implementation time). During **breaks**, when the timer is **stopped**, or on **abnormal exit** (SIGINT, kill), that extra layer is **removed**. Permanent distraction blocking already provided by NixOS `networking.extraHosts` (`hosts.txt`) **remains unchanged** and **continues to apply at all times**.

## Background (current behavior)

- **Xmobar** (`modules/home/xmobar/xmobarrc_top.hs`) runs `/home/ben/.bin/pomodoro-control` on left-click over the Pomodoro segment.
- **`pomodoro-control.sh`** starts `~/.bin/pomodoro` if no process is running, otherwise kills the Pomodoro process and resets the status file.
- **`pomodoro.sh`** loops: work countdown → break countdown → repeat. SIGINT resets status and exits.

## Goals

1. **Work-only blocking:** When a **work** phase starts, enable extra DNS blocks from the user blocklist. When the work phase ends (transition to **break**), disable those blocks.
2. **Clean teardown:** When Pomodoro **stops** (second click, kill, Ctrl+C), ensure the extra blocks are **off**.
3. **True DNS-level blocking** for normal applications that use the system resolver (same class of control as hosts-based blocking, not browser-only).
4. **User-maintained list:** Blocklist lives under home, editable without committing to the dotfiles repo or running `nixos-rebuild`.
5. **Coexistence:** Do not replace or duplicate the role of `networking.extraHosts`; the new layer is **additive during work** only.

## Non-goals

- Blocking traffic that **bypasses** system DNS (e.g. some app-specific DoH configurations).
- Per-browser policies, extension-based focus, or HTTP proxy blocking (out of scope).
- Automatic migration of entries between `hosts.txt` and the Pomodoro blocklist (operator may reorganize manually).

## Architecture

### Components

1. **NixOS networking / dnsmasq**
   - Run a **local DNS forwarder** (recommended: **dnsmasq**) on `127.0.0.1`, forwarding to chosen upstream resolvers (e.g. Cloudflare / Quad9, consistent with existing comments in `modules/system/networking/default.nix`).
   - Configure the system (via **NetworkManager** and/or global `networking.nameservers`) so normal resolution goes through this forwarder.
   - Provide an **`include`** (or equivalent) for a **focus-only fragment** file managed by the helper. When empty or absent, behavior is “forward only, no extra sinkholes.”

2. **Privileged DNS helper** (single entrypoint, e.g. `pomodoro-dns-apply`)
   - **Input:** Mode or arguments such as `on` / `off`, and/or path to the user blocklist (fixed path is acceptable).
   - **Behavior:**
     - **`on`:** Read the user blocklist; generate dnsmasq snippet lines (e.g. `address=/hostname/0.0.0.0` per non-comment, non-empty line); write the **system** focus fragment atomically; **`systemctl reload`** (or equivalent) dnsmasq.
     - **`off`:** Clear the focus fragment (or write an empty valid config); reload dnsmasq.
   - **Invocation:** Called from Pomodoro shell scripts via **`sudo`** with a **tightly scoped** sudoers rule allowing **only** this helper (and optionally the exact reload), not general root.

3. **Pomodoro script hooks**
   - **`pomodoro.sh`:** After work starts (immediately before or after existing “work session started” behavior), call helper **`on`**. When work countdown **finishes** (before break), call **`off`**. When starting work again after a break, call **`on`** again. In **`ctrl_c` trap** and any exit path, call **`off`**.
   - **`pomodoro-control.sh`:** When **killing** the Pomodoro process, call **`off`** so DNS state does not leak after the bar stops the timer.
   - Hooks should be **idempotent where practical** (e.g. `on` regenerates from file; `off` clears).

### Blocklist file format

- **Path:** Under `~/.config/pomodoro/` (exact filename fixed at implementation; e.g. `blocklist.txt`).
- **Format:** One **hostname** per line; lines starting with `#` are comments; blank lines ignored.
- **Missing file:** Treat as **no extra blocks** (optional: one low-urgency notification).

### Interaction with `/etc/hosts` (`extraHosts`)

- NixOS continues to manage **static** `hosts.txt` via `networking.extraHosts`.
- **nsswitch** typically consults `files` before DNS; permanent blocks in `hosts.txt` remain effective regardless of Pomodoro phase.
- The Pomodoro layer adds **dnsmasq-level** sinkholes only for names in the user blocklist, **during work**.

## Data flow

1. User edits `~/.config/pomodoro/blocklist.txt`.
2. Work phase starts → script runs `pomodoro-dns-apply on` → helper reads file → writes focus fragment → reloads dnsmasq.
3. Queries for listed names are answered by the forwarder as sinkholed (implementation detail: `0.0.0.0` or as chosen for dnsmasq).
4. Work ends / break starts → `pomodoro-dns-apply off` → fragment cleared → reload.
5. Stop or interrupt → `off` from control script or trap.

## Error handling

- **Empty blocklist:** `on` is a no-op at the DNS layer (empty fragment).
- **Reload failure:** Surface via **`dunstify`** (or stderr); default policy: **warn and continue** Pomodoro (implementation may tighten this if desired).
- **Invalid lines:** Skip lines that are not sensible hostnames; optionally log count of skipped lines once per `on`.

## Security and privilege notes

- The helper runs as **root** and must **validate** input: only allow hostname-shaped tokens; reject paths, shell metacharacters, and excessive line counts to avoid oversized configs.
- Sudoers grants **only** the packaged helper path, not arbitrary commands.

## Testing (manual)

- With Pomodoro **stopped**, confirm baseline resolution works through dnsmasq.
- Start work: `dig` / `getent hosts` (or browser) against a name **only** in the user blocklist → blocked; name **only** in `hosts.txt` → still blocked in break if so configured.
- Enter break: user blocklist names **resolve** again (unless also in `hosts.txt`).
- Stop Pomodoro: same as break for user blocklist behavior.

## Open items for implementation plan

- Exact NixOS options for dnsmasq (`services.dnsmasq` or successor) and NetworkManager integration on the target NixOS version.
- Exact filesystem location for the focus fragment (`/var/lib/...` vs `/run/...`) and package layout for the helper (system package vs activation script).
- Whether to ensure `~/.config/pomodoro/` exists via home-manager with a **`.example`** blocklist (optional).

## Approval

Design agreed in session: work-only scope, dnsmasq + dynamic include, user home blocklist, preserve `extraHosts`.
