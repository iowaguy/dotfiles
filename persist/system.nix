{ config, pkgs, lib, ... }:

let
  sources = import ../nix/sources.nix {};
in {
  # boot.initrd.postDeviceCommands = lib.mkAfter ''
  #   zfs rollback -r rpool/local/root@blank
  # '';

  imports = [ "${sources.impermanence}/nixos.nix" ];

  # The following sections are for persisting data after reboots: https://grahamc.com/blog/erase-your-darlings
  systemd.tmpfiles.rules = [
    "L /etc/NetworkManager/system-connections - - - - /persist/etc/NetworkManager/system-connections/"
    "L /home/ben/workspace - - - - /persist/home/ben/workspace"
    "L /home/ben/.gnupg - - - - /persist/home/ben/.gnupg"
    "L /home/ben/.ssh - - - - /persist/home/ben/.ssh"
    "L /home/ben/.config/Signal - - - - /persist/home/ben/.config/Signal"
    "L /home/ben/.config/syncthing - - - - /persist/home/ben/.config/syncthing"
    "L /home/ben/.emacs.d - - - - /persist/home/ben/.emacs.d"
    "L /home/ben/.mozilla - - - - /persist/home/ben/.mozilla"
    "L /home/ben/.local/share/direnv - - - - /persist/home/ben/.local/share/direnv"
    "L /home/ben/.local/share/keyrings - - - - /persist/home/ben/.local/share/keyrings"
    "L /home/ben/.local/share/tridactyl - - - - /persist/home/ben/.local/share/tridactyl"
    "L /home/ben/.config/skypeforlinux - - - - /persist/home/ben/.config/skypeforlinux"
    "L /home/ben/.config/Slack - - - - /persist/home/ben/.config/Slack"
    "L /home/ben/.vagrant.d - - - - /persist/home/ben/.vagrant.d"
    "L /home/ben/.terraform.d - - - - /persist/home/ben/.terraform.d"
    "L /home/ben/.packer.d - - - - /persist/home/ben/.packer.d"
    "L /home/ben/.password-store - - - - /persist/home/ben/.password-store"
    "L /home/ben/Zotero - - - - /persist/home/ben/Zotero"
  ];
  ## Don't need this b/c this desktop doesn't support bluetooth.
  # systemd.tmpfiles.rules = [
  #   "L /var/lib/bluetooth - - - - /persist/var/lib/bluetooth"
  # ];
  services.zfs.autoSnapshot.enable = true;
  services.openssh = {
    enable = true;
    hostKeys = [
      {
        path = "/persist/etc/ssh/ssh_host_ed25519_key";
        type = "ed25519";
      }
      {
        path = "/persist/etc/ssh/ssh_host_rsa_key";
        type = "rsa";
        bits = 4096;
      }
    ];
  };
  # environment.persistence."/persist" = {
  #   directories = [
  #     # "/var/lib/bluetooth"
  #     # "/var/lib/systemd/coredump"
  #     "/etc/NetworkManager/system-connections"
  #     "/home/ben/workspace"
  #     "/home/ben/.gnupg"
  #     "/home/ben/.ssh"
  #     "/home/ben/.config/Signal"
  #     "/home/ben/.config/syncthing"
  #     "/home/ben/.emacs.d"
  #     "/home/ben/.mozilla"
  #     "/home/ben/.local/share/direnv"
  #     "/home/ben/.local/share/keyrings"
  #     "/home/ben/.local/share/tridactyl"
  #     "/home/ben/.config/skypeforlinux"
  #     "/home/ben/.config/Slack"
  #     "/home/ben/.vagrant.d"
  #     "/home/ben/.terraform.d"
  #     "/home/ben/.packer.d"
  #     "/home/ben/.password-store"
  #     "/home/ben/Zotero"
  #   ];
  #   files = [
  #     "/etc/machine-id"
  #     "/etc/nix/id_rsa"
  #   ];
  #   allowOther = true;
  # };
}
