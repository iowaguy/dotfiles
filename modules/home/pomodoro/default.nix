{ config, lib, pkgs, ... }:

{
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
