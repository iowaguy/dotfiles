{pkgs, lib, config, ...}:
{

  # systemd.user = {
  #   services = {
  #     taffybar = {
  #       Install = {
  #         WantedBy = ["tray.target"];
  #       };
  #       Service = {
  #         BusName = "org.taffybar.Bar";
  #         Restart = "on-failure";
  #         Type = "dbus";
  #         ExecStart = ''
  #           ${pkgs.taffybar}/bin/taffybar --log-level DEBUG
  #           '';
  #       };
  #       Unit = {
  #         Description = "Taffybar desktop bar";
  #         PartOf = "tray.target";
  #       };
  #     };
  #   };
  # };
  home.packages = with pkgs; [
    taffybar                             # A cool status bar
    haskellPackages.gtk-sni-tray         # gtk-sni-tray-standalone, needed for taffybar https://github.com/taffybar/taffybar/issues/355
    haskellPackages.status-notifier-item # status-notifier-watcher for, needed for taffybar https://github.com/taffybar/taffybar/issues/355
    hicolor-icon-theme                   # Needed to get Network Manager applet working
    gnome-icon-theme                     # Needed to get Network Manager applet working
  ];
  xdg.configFile."taffybar/taffybar.hs".source = ./taffybar.hs;
  services.taffybar.enable = true;
}
