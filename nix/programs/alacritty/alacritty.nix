{ pkgs, ... }:

{
  home.file.".local/share/applications/alacritty.desktop".source = "${builtins.getEnv "HOME"}/workspace/dotfiles/nix/programs/alacritty/alacritty.desktop";

  programs.alacritty = {
    enable = true;
    settings = {
      shell.program = /run/current-system/sw/bin/zsh;
    };
  };

}
