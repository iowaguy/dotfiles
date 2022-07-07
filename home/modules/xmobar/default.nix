{pkgs, lib, config, ...}:
{
  home.packages = with pkgs; [ xmobar ];
  xdg.configFile."xmobar/xmobarrc".source = ./xmobarrc.hs;
  # xdg.configFile."xmobar/xmobarrc2".source = ./xmobarrc;
}
