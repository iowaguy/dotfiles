{ config, lib, pkgs, ... }:

{

  home.packages = with pkgs; [
    dunst
  ];

  services.dunst = {
    enable = true;
    settings = {
      global = {
        geometry = "500x50-30+50";
        follow = "keyboard";
        transparency = 0;
        frame_color = "#ff0000";
        frame_width = 5;
        font = "Droid Sans 30";
      };

      urgency_critical = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
      urgency_normal = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
      urgency_low = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout = 10;
      };
   };
  };
}
