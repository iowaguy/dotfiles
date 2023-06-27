{ config, lib, pkgs, ... }:

{
  boot.crashDump.enable = true;
  systemd.services.upower.enable = true;
  services = {
    gnome.gnome-keyring.enable = true;
    openssh.enable = true; # Enable the OpenSSH daemon.
    redshift.enable = true; # Save the eyes

    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };

    xserver = {
      enable = true;
      layout = "us";

      desktopManager = {
        wallpaper.mode = "scale";
        xterm.enable = false;
        plasma5 = {
          enable = true;
          runUsingSystemd = true;
        };
      };

      libinput = {
        enable = true;
        touchpad = {
          disableWhileTyping = true;
          naturalScrolling = true;
        };
        mouse = {
          disableWhileTyping = true;
          naturalScrolling = true;
        };
      };

      displayManager = {
        defaultSession = "plasma";
        sessionCommands = ''
          # This command sets the background image for the session
          feh --bg-fill ~/.background-image
        '';
      };
      #displayManager.defaultSession = "none+xmonad";

      windowManager.xmonad = {
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
  };
}
