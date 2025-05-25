{ config, lib, pkgs, ... }:

{
  home.file = {
    ".bin/noise" = {
      source = ./noise;
      executable = true;
    };
    ".bin/noise-control" = {
      source = ./noise-control.sh;
      executable = true;
    };
  };
}
