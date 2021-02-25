{ config, lib, pkgs, ... }:

{
  services = {
    gnome3.gnome-keyring.enable = true;
    openssh.enable = true; # Enable the OpenSSH daemon.
    redshift.enable = true; # Save the eyes
    upower.enable = true;

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
      };

      windowManager.i3 = {
        enable = true;
        extraPackages = with pkgs; [
          dmenu #application launcher most people use
          i3status # gives you the default i3 status bar
          i3lock #default i3 screen locker
          i3blocks #if you are planning on using i3blocks over i3status
        ];
      };
    };
  };
}
