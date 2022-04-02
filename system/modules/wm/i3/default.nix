{ config, lib, pkgs, ... }:

{

  boot.crashDump.enable = true;
  systemd.services.upower.enable = true;
  services = {
    das_watchdog.enable = true;
    gnome.gnome-keyring.enable = true;
    openssh.enable = true; # Enable the OpenSSH daemon.
    redshift.enable = true; # Save the eyes

    # dbus = {
    #   enable = true;
    #   socketActivated = true;
    #   packages = [ pkgs.gnome3.dconf ];
    # };

    xserver = {
      enable = true;
      # startDbusSession = true;
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
        defaultSession = "xfce+i3";
        sessionCommands = ''
          # This command sets the background image for the session
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
