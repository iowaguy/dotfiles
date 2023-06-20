{ config, lib, pkgs, ... }:

{
  home.file.".bin/noise" = {
    source = ./noise;
    executable = true;
  };
}
