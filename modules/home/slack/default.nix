{ pkgs, config, ... }:

{
  home.packages = with pkgs; [
    slack
  ];

  xdg.mimeApps = {
    associations.added = {
      "x-scheme-handler/slack" = [ "slack.desktop" ];
    };
    defaultApplications = {
      "x-scheme-handler/slack" = [ "slack.desktop" ];
    };
  };
}
