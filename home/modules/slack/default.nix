{ pkgs, ... }:

{
  home.packages = with pkgs; [
    slack
  ];

  home.file.".local/share/applications/slack.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec=${builtins.getEnv "HOME"}/.nix-profile/bin/slack
    Name=slack
  '';

  xdg.mimeApps = {
    associations.added = {
      "x-scheme-handler/slack" = [ "slack.desktop" ];
    };
    defaultApplications = {
      "x-scheme-handler/slack" = [ "slack.desktop" ];
    };
  };
}
