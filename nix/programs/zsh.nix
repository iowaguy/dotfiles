{ pkgs, ... }:

{
  home.file.".zshrc".text = ''
    eval "$(direnv hook zsh)"
  '';
}
