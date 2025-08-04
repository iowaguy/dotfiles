{ pkgs, ... }:

{
  boot.crashDump.enable = true;
  systemd.services.upower.enable = true;
  services = {
    gnome.gnome-keyring.enable = true;
    openssh.enable = true; # Enable the OpenSSH daemon.

    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
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

    desktopManager = {
      plasma6.enable = true;
    };

    xserver = {
      enable = true;
      xkb.layout = "us";
      upscaleDefaultCursor = true;

      desktopManager = {
        wallpaper.mode = "scale";
        xterm.enable = false;
      };
      displayManager = {
        sessionCommands = ''
          # This command sets the background image for the session
          feh --bg-fill ~/.background-image
        '';
        lightdm.enable = true; # Or lightdm.enable, gdm.enable, sddm.enable, etc.
      };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = with pkgs; f: [
          dmenu #application launcher most people use
          acpi
          feh
          python3
          networkmanager_dmenu   # networkmanager on dmenu
          networkmanagerapplet   # networkmanager applet

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
