{ config, lib, pkgs, ... }:

{
  # When I switch to a directory, I want it to assume a certain
  # environment.
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  home.file.".bin/shell-template.nix" = {
    source = ./shell-template.nix;
  };
}
