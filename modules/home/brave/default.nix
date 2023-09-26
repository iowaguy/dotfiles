{ pkgs, pkgsUnstable, ... }:

{
  home.packages = with pkgsUnstable; [
    brave
  ];

  programs.zsh.localVariables.BROWSER = "brave";

  xdg.mimeApps = {
    associations.added = {
      "x-scheme-handler/http" = [ "brave-browser.desktop" ];
      "x-scheme-handler/https" = [ "brave-browser.desktop" ];
      "text/html" = [ "brave-browser.desktop" ];
    };
    defaultApplications = {
      "x-scheme-handler/http" = [ "brave-browser.desktop" ];
      "x-scheme-handler/https" = [ "brave-browser.desktop" ];
      "text/html" = [ "brave-browser.desktop" ];
    };
  };
}
