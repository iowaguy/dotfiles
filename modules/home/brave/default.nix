{ pkgs, pkgsUnstable, ... }:

{
  home.packages = with pkgsUnstable; [
    brave
  ];

  programs.zsh.localVariables.BROWSER = "brave";

  xdg.mimeApps = {
    associations.added = {
      "x-scheme-handler/http" = [ "brave.desktop" ];
      "x-scheme-handler/https" = [ "brave.desktop" ];
      "text/html" = [ "brave.desktop" ];
    };
    defaultApplications = {
      "x-scheme-handler/http" = [ "brave.desktop" ];
      "x-scheme-handler/https" = [ "brave.desktop" ];
      "text/html" = [ "brave.desktop" ];
    };
  };
}
