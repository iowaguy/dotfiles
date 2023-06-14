{pkgs, lib, config, ...}:
{
  home.packages = with pkgs; [
    brightnessctl
    xdotool # needed for clickable workspaces
  ];

  xsession = {
    enable = true;

    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = hp: [
        hp.dbus
        hp.monad-logger
        hp.xmonad-contrib
      ];
      config = ./config.hs;
    };
  };
}
