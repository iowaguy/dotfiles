{ config, lib, pkgs, ... }:

{
  # When I switch to a directory, I want it to assume a certain
  # environment.
  programs.direnv = {
    enable = true;
    enableFishIntegration = true;
    nix-direnv = {
      enable = true;
      # optional for nix flakes support
      enableFlakes = true;
    };
  };

  programs.fish.shellInit = ''
    function nixify
      if ! test -e ./.envrc
        echo "use nix"\n"unset PS1" > .envrc
        direnv allow
      end
      if ! test -e shell.nix && ! test -e default.nix
        cp $HOME/.bin/shell-template.nix shell.nix
        chmod u+w shell.nix
        $EDITOR shell.nix
      end
    end

    function flakify
      if ! test -e flake.nix
        nix flake new -t github:nix-community/nix-direnv .
      elif ! test -e .envrc
        echo "use flake" > .envrc
        direnv allow
      end
      $EDITOR flake.nix
    end
  '';

  home.file.".bin/shell-template.nix" = {
    source = ./shell-template.nix;
  };
}
