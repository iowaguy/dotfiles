{pkgs, lib, config, ...}:

{
  home.packages = with pkgs; [
    stalonetray # a tray to hold app icons
  ];
  xdg.configFile."xmobar/xmobarrc_top".source = ./xmobarrc_top.hs;
  xdg.configFile."xmobar/xmobarrc_bottom".source = ./xmobarrc_bottom.hs;

  home.file.".stalonetrayrc".source = ./stalonetrayrc;
}
