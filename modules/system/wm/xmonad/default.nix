{ config, lib, pkgs, ... }:

{
  services = {
    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = with pkgs; f: [
        dmenu #application launcher most people use
        i3lock #default i3 screen locker
        i3blocks #if you are planning on using i3blocks over i3status
        acpi
        feh
        python3
        networkmanager_dmenu   # networkmanager on dmenu
        networkmanagerapplet   # networkmanager applet

        # All needed for betterlockscreen
        betterlockscreen
        i3lock-color
        imagemagick
        xorg.xdpyinfo
        bc

        # for cpu_usage in i3blocks
        sysstat
        perl

        # Messaging used by xmonad
        xorg.xmessage
      ];
    };
  };
}
