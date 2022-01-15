{ pkgs, ... }:

{
  xdg.configFile."i3/config".source = ./i3-config.ini;
  xdg.configFile."i3/isec-desktop-config.ini".source = ./isec-desktop-config.ini;
  xdg.configFile."i3/kansas-config.ini".source = ./kansas-config.ini;
  xdg.configFile."i3/x1-2021-config.ini".source = ./x1-2021-config.ini;
}
