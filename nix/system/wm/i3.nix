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
        xterm.enable = false;
      };

      libinput = {
        enable = true;
        disableWhileTyping = true;
        naturalScrolling = true;
      };

      displayManager = {
        defaultSession = "none+i3";
        sessionCommands = ''
          xrandr --output eDP --primary
          xrandr --output HDMI-0 --auto --right-of eDP
          xrandr --newmode "3840x2160_60.00"  712.75  3840 4160 4576 5312  2160 2163 2168 2237 -hsync +vsync
          xrandr --addmode HDMI-0 3840x2160_60.00
          # xrandr --output HDMI-0 --mode "3840x2160_60.00"

          feh --bg-scale ~/.background_image
          betterlockscreen --update ~/.background_image
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
