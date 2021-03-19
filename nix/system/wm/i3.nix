{ config, lib, pkgs, ... }:

{
  systemd.services.upower.enable = true;
  services = {
    gnome3.gnome-keyring.enable = true;
    openssh.enable = true; # Enable the OpenSSH daemon.
    redshift.enable = true; # Save the eyes
    upower.enable = true;
    blueman.enable = true;

    xserver = {
      enable = true;
      startDbusSession = true;
      layout = "us";

      desktopManager = {
        wallpaper.mode = "scale";
        xterm.enable = false;
        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
        };
      };

      libinput = {
        enable = true;
        disableWhileTyping = true;
        naturalScrolling = true;
      };

      displayManager = {
        defaultSession = "xfce+i3";
        sessionCommands = ''
          feh --bg-fill ~/.background-image
        '';
      };

      windowManager.i3 = {
        package = pkgs.i3-gaps;
        enable = true;
        extraPackages = with pkgs; [
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
        ];
      };
    };
  };
}
