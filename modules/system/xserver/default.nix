{ config, pkgs, ... }:

{
  services.xserver = {
    enable = true;
    layout = "us";
    config = ''
      Option "BlankTime" "10"
    '';

    displayManager = {
      sddm.enable = true;
      defaultSession = "plasma";
      sessionCommands = ''
        # This command sets the background image for the session
        feh --bg-fill ~/.background-image

        # Suspends and locks session on lid close
        xfconf-query -c xfce4-session -p /general/LockCommand -s "systemctl suspend";
      '';
    };

    # keyboard settings
    xkbOptions = "altwin:swap_lalt_lwin"; # Swap left alt with left win

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

        # one, two, three fingered clicks (on touchpad) map to left, right, middle clicks
        clickMethod = "clickfinger";
      };
      mouse = {
        disableWhileTyping = true;
        naturalScrolling = true;
      };
    };
  };
}
