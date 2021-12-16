{ config, pkgs, lib, impermanence ? import (import ../nix/sources.nix).impermanence {}, ... }:

{
  boot.initrd.postDeviceCommands = lib.mkAfter ''
    zfs rollback -r rpool/local/root@blank
  '';

  imports = [ "${impermanence}/nixos.nix" ];

  # The following sections are for persisting data after reboots: https://grahamc.com/blog/erase-your-darlings
  etc."NetworkManager/system-connections" = {
    source = "/persist/etc/NetworkManager/system-connections/";
  };
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
}
