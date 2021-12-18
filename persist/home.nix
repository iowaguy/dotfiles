{ config, pkgs, ... }:
let
  sources = import /persist/home/ben/workspace/dotfiles/nix/sources.nix {};
in {
  imports = [ "${sources.impermanence}/home-manager.nix" ];

  # Things that should get real backups.
  # Heuristic: are the source of truth and important.
  # So: keys, cloud-based things where local changes might need to be recovered
  # (Syncthing/code)
}
