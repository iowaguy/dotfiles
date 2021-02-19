{ pkgs, ... }:

{
  home.file.".local/share/applications/zotero.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec=${builtins.getEnv "HOME"}/.nix-profile/bin/zotero
    Name=zotero
    Icon=${builtins.getEnv "HOME"}/.nix-profile/share/icons/hicolor/256x256/apps/zotero.png
  '';
}
