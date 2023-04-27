{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    dunst
  ];

  services.dunst = {
    enable = true;
    settings = {
      global = {
        follow = "keyboard";
        transparency = 5;
        frame_color = "#ff0000";
        frame_width = 1;
        font = "Droid Sans 15";
        width = 300;
        height = 300;
        origin = "top-right";
        scale = 0;
        radius = 0;
        notification_limit = 0;
        progress_bar = true;
        progress_bar_height = 10;
        progress_bar_frame_width = 1;
        progress_bar_min_width = 150;
        progress_bar_max_width = 300;
        indicate_hidden = true;
        separator_height = 2;
        padding = 6;
        horizontal_padding = 6;
        markup = "full";
        title = "Dunst";
        class = "Dunst";
      };

      urgency_critical = {
        frame_color = "#B7472A";
        foreground = "#B7472A";
        background = "#191311";
        timeout = 10;
      };
      urgency_normal = {
        frame_color = "#5B8234";
        foreground = "#5B8234";
        background = "#191311";
        timeout = 8;
      };
      urgency_low = {
        frame_color = "#3B7C87";
        foreground = "#3B7C87";
        background = "#191311";
        timeout = 4;
      };
   };
  };
}
