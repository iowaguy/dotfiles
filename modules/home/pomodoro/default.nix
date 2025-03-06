{ config, lib, pkgs, ... }:

{
  home.file = {
    ".bin/pomodoro" = {
      source = ./pomodoro.sh;
      executable = true;
    };
    ".bin/pomodoro-control" = {
      source = ./pomodoro-control.sh;
      executable = true;
    };
  };
}
