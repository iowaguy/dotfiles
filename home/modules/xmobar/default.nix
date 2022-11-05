{pkgs, lib, config, ...}:
{
  home.packages = with pkgs; [
    # xmobar
    stalonetray # a tray to hold app icons
  ];
  xdg.configFile."xmobar/xmobarrc_top".source = ./xmobarrc_top.hs;
  xdg.configFile."xmobar/xmobarrc_bottom".source = ./xmobarrc_bottom.hs;

  # xdg.configFile."xmobar/xmobarrc2".source = ./xmobarrc;
  home.file.".stalonetrayrc".source = ./stalonetrayrc;
}
