{ pkgs, ... }:

with import /persist/home/ben/workspace/areas/system-management/dotfiles/nix/sources.nix {};
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
        BROWSER = "firefox";
        PATH = "$HOME/.bin:$HOME/.emacs.d/bin:$PATH";
        EDITOR = "emacsclient";
      };

      shellAliases = {
        ls = "exa";
        lla = "exa -la";
        cat = "bat";
        grep = "rg";
        xclip = "xclip -selection clipboard";
        hms = "home-manager switch";
        myr = "mynix refresh";
      };

      prezto = {
        enable = true;
        git.submoduleIgnore = "all";
        prompt = {
          theme = "powerline";

          # TODO tying uncommenting this after upgrading to 23.05
          # showReturnVal = true;
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
      '';
    };
  };

}
