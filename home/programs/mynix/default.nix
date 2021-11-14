{ config, lib, pkgs, ... }:

{
  home.file.".bin/mynix" = {
    source = ./mynix;
    executable = true;
  };
}
