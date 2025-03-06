{ config, lib, pkgs, ... }:

{
  home.file.".bin/pomodoro" = {
    source = ./pomodoro.sh;
    executable = true;
  };
}
