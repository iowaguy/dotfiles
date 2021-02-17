{ pkgs, ... }:

{
  home.file.".local/share/applications/zoom.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec=/home/ben/.nix-profile/bin/zoom
    Name=zoom
  '';
}
