{ config, lib, pkgs, ... }:

{
  # When I switch to a directory, I want it to assume a certain
  # environment.
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv = {
      enable = true;
      # optional for nix flakes support
      enableFlakes = true;
    };
  };

  programs.zsh.initExtra = ''
    #!/bin/sh

    nixify() {
      if [ ! -e ./.envrc ]; then
        echo "use nix\nunset PS1" > .envrc
        direnv allow
      fi
      if [[ ! -e shell.nix ]] && [[ ! -e default.nix ]]; then
        cp $HOME/.bin/shell-template.nix shell.nix
        ''${EDITOR:-vim} shell.nix
      fi
    }

    flakify() {
      if [ ! -e flake.nix ]; then
        nix flake new -t github:nix-community/nix-direnv .
      elif [ ! -e .envrc ]; then
        echo "use flake" > .envrc
        direnv allow
      fi
      ''${EDITOR:-vim} flake.nix
    }
  '';

  home.file.".bin/shell-template.nix" = {
    source = ./shell-template.nix;
  };
}
