{ config, pkgs, ... }:
let
  sources = import /home/ben/workspace/dotfiles/nix/sources.nix {};
in {
  imports = [ "${sources.impermanence}/home-manager.nix" ];

  # Things that should get real backups.
  # Heuristic: are the source of truth and important.
  # So: keys, cloud-based things where local changes might need to be recovered
  # (Syncthing/code)
  home.persistence."/persist/ben" = {
    directories = [
      "workspace"
      ".gnupg"
      ".ssh"
      ".config/Signal"
      ".config/syncthing"
      ".emacs.d"
      ".mozilla"
      ".local/share/direnv"
      ".local/share/keyrings"
      ".local/share/tridactyl"
      ".config/skypeforlinux"
      ".config/Slack"
      ".vagrant.d"
      ".terraform.d"
      ".packer.d"
      ".password-store"
      "Zotero"
    ];
    allowOther = true;
  };
}
