{ pkgs, ... }:

{
  home.file.".zshrc".text = ''
    eval "$(direnv hook zsh)"
  '';

  programs.zsh = {
    enable = true;
    sessionVariables = {
      BROWSER="firefox";
      PATH = "$HOME/.bin:$PATH";
      EDITOR = "emacsclient";
    };
  };
}
