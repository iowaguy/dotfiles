{ pkgs, ... }:

{
  home.file.".zshrc".text = ''
    eval "$(direnv hook zsh)"
  '';

  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    sessionVariables = {
      BROWSER="firefox";
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
