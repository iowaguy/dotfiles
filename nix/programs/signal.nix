{ pkgs, ... }:

{
  home.file.".local/share/applications/signal.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec=${builtins.getEnv "HOME"}/.nix-profile/bin/signal-desktop
    Name=signal
    Icon=${builtins.getEnv "HOME"}/.nix-profile/share/icons/hicolor/1024x1024/apps/signal-desktop.png
  '';
}
