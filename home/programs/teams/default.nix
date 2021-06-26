{ pkgs, ... }:

{
  home.packages = with pkgs; [
    teams
  ];

  home.file.".local/share/applications/teams.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec=${builtins.getEnv "HOME"}/.nix-profile/bin/teams
    Name=teams
  '';
}
