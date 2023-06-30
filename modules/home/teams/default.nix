{ pkgs, config, ... }:

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
    Exec=${config.home.profileDirectory}/bin/teams
    Name=teams
  '';

  xdg.mimeApps.associations.added = {
    "x-scheme-handler/msteams" = [ "teams.desktop" ];
  };
}
