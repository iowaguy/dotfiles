{ config, lib, pkgs, ... }:

{
  # Hostnames sinkholed only during Pomodoro *work* phases when pomodoroFocusDns is enabled.
  home.file.".config/pomodoro/blocklist.txt" = {
    force = true;
    text = ''
      # One hostname per line. Lines starting with # are ignored.
      # example.com
    '';
  };

  home.file = {
    ".bin/pomodoro" = {
      source = ./pomodoro.sh;
      executable = true;
    };
    ".bin/beep.wav" = {
      source = ./beep.wav;
      executable = false;
    };
    ".bin/pomodoro-control" = {
      source = ./pomodoro-control.sh;
      executable = true;
    };
  };
}
