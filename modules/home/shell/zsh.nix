{ pkgs, ... }:

{
  home.packages = with pkgs; [
    inconsolata # font
    bat # A better cat
    ripgrep # A better grep
    zsh-prezto # some useful commands for interacting with prezto
  ];

  programs = {
    fzf.enableZshIntegration = true;
    zoxide.enableZshIntegration = true;
    broot.enableZshIntegration = true;
    direnv.enableZshIntegration = true;

    zsh = {
      enable = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
      enableVteIntegration = true;

      dotDir = ".config/zsh";

      localVariables = {
        PATH = "$HOME/.bin:$HOME/.emacs.d/bin:$PATH";
        EDITOR = "emacsclient";
      };

      shellAliases = rec {
        ls = "exa";
        lla = "exa -la";
        la = lla;
        cat = "bat";
        grep = "rg";
        xclip = "xclip -selection clipboard";
      };

      prezto = {
        enable = true;
        git.submoduleIgnore = "all";
        prompt = {
          theme = "powerline";
          showReturnVal = true;
        };
        pmodules = [
          "git"
          "prompt"
        ];
      };

      initExtra = ''
        #!/bin/sh

        nixify() {
          if [ ! -e ./.envrc ]; then
            echo "use nix\nunset PS1" > .envrc
            direnv allow
          fi
          if [[ ! -e shell.nix ]] && [[ ! -e default.nix ]]; then
            cp $HOME/.bin/shell-template.nix shell.nix
            chmod u+w shell.nix
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

        fsw() {
          if [[ ! -L "$HOME/.config/mimeapps.list" ]]; then
            echo "Removing non-symlinked mimeapps.list"
            rm -f "$HOME/.config/mimeapps.list"
            echo "Exit status of rm is $?"
          fi
          echo -n "mimeapps.list should be a symlink or empty:"
          echo "$(ls -l1 $HOME/.config/mimeapps.list)"
          sudo nixos-rebuild --flake "$HOME/workspace/areas/system-management/dotfiles/.#" switch "$@"
        }
      '';
    };
  };

}