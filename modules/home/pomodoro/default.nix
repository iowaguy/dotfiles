{ config, lib, pkgs, ... }:

{
  home.file = {
    # Hostnames sinkholed only during Pomodoro *work* phases when pomodoroFocusDns is enabled.
    ".config/pomodoro/blocklist.txt" = {
      force = true;
      text = ''
        # One hostname per line. Lines starting with # are ignored.
        # example.com
      '';
    };
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
