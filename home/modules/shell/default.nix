{ pkgs, ... }:

with import /persist/home/ben/workspace/dotfiles/nix/sources.nix {};
{
  home.packages = with pkgs; [
    inconsolata # font

    bat # A better cat
    ripgrep # A better grep
  ];


  programs.fish = {
    enable = true;
    # enableAutosuggestions = true;
    shellInit = ''
      export BROWSER="firefox";
      export PATH="$HOME/.bin:$HOME/.emacs.d/bin:$PATH";
      export EDITOR="emacsclient";
    '';

    shellAliases = {
      ls = "exa";
      cat = "bat";
      grep = "rg";
      xclip = "xclip -selection clipboard";
      hms = "home-manager switch";
      myr = "mynix refresh";
    };

    plugins = [
      {
        name = "theme-bobthefish";
        src = pkgs.fetchFromGitHub {
          owner = theme-bobthefish.owner;
          repo = theme-bobthefish.repo;
          rev = theme-bobthefish.rev;
          sha256 = theme-bobthefish.sha256;
        };
      }

      {
        name = "colored_man_pages.fish";
        src = pkgs.fetchFromGitHub {
          owner = colored_man_pages.owner;
          repo = colored_man_pages.repo;
          rev = colored_man_pages.rev;
          sha256 = colored_man_pages.sha256;
        };
      }
    ];
  };
}
