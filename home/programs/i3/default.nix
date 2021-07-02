{ pkgs, ... }:

{
  xdg.configFile."i3/config".source = ./i3-config.ini;
#  xdg.configFile."i3blocks/config".source = ./i3blocks-config.ini;
#  xdg.configFile."i3blocks/battery".source = ./battery.py;
#  xdg.configFile."i3blocks/cpu_usage".source = ./cpu_usage.pl;
}
