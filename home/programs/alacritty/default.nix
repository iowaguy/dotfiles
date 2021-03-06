{ pkgs, ... }:

{
  home.packages = with pkgs; [
    alacritty
    inconsolata  # font
  ];

  # Make a desktop icon so I can select it frome gnome search
  home.file.".local/share/applications/alacritty.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec="${builtins.getEnv "HOME"}/.nix-profile/bin/alacritty
    Name=alacritty
    Icon="${builtins.getEnv "HOME"}/.nix-profile/share/icons/hicolor/scalable/apps/Alacritty.svg
  '';

  programs.alacritty = {
    enable = true;
    settings = {
      shell.program = "/run/current-system/sw/bin/zsh";
      background_opacity = 0.9;
    };
  };

}
