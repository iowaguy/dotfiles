{ pkgs, ... }:

{
  home.file.".local/share/applications/teams.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec=${builtins.getEnv "HOME"}/.nix-profile/bin/teams
    Name=teams
    # Icon=${builtins.getEnv "HOME"}/.nix-profile/share/icons/hicolor/1024x1024/apps/signal-desktop.png
  '';
}
