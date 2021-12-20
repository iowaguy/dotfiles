{ config, pkgs, ... }:
let
  sources = import /persist/home/ben/workspace/dotfiles/nix/sources.nix {};
in {
  imports = [ "${sources.impermanence}/home-manager.nix" ];

  # Things that should get real backups.
  # Heuristic: are the source of truth and important.
  # So: keys, cloud-based things where local changes might need to be recovered
  # (Syncthing/code)
  home.persistence."/persist/home/ben" = {
    directories = [
      "workspace"
      "Zotero"
      ".mozilla"
      ".emacs.d"
      ".gnupg"
      ".ssh"
      ".config/Signal"
      ".config/skypeforlinux"
      ".config/Slack"
      ".config/syncthing"
      ".local/share/direnv"
      ".local/share/keyrings"
      ".local/share/tridactyl"
      ".packer.d"
      ".password-store"
      ".terraform.d"
      ".vagrant.d"
    ];
    allowOther = true;
  };
}
