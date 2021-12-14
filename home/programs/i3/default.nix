{ pkgs, ... }:

{
  xdg.configFile."i3/config".source = ./i3-config.ini;
}
