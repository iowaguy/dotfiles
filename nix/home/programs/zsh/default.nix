{ pkgs, ... }:

{
  home.packages = with pkgs; [
    alacritty
    inconsolata # font

    exa # A better ls
    bat # A better cat
    ripgrep # A better grep
  ];

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    sessionVariables = {
      BROWSER = "firefox";
      PATH = "$HOME/.bin:$PATH";
      EDITOR = "emacsclient";
    };

    shellAliases = {
      ls = "exa";
      cat = "bat";
      grep = "rg";
    };
  };
}
