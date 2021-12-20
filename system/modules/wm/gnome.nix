{ config, lib, pkgs, ... }:

{
  # Enable the GNOME 3 Desktop Environment.
  services = {
    xserver = {
      enable = true;
      displayManager.gdm.enable = true;
      desktopManager.gnome3.enable = true;
    };

    blueman.enable = true;

    # Enable the OpenSSH daemon.
    openssh.enable = true;
    redshift.enable = true;
  };
}
