{ config, lib, pkgs, ... }:

{
  systemd.user.services.betterlockscreen = {
    Unit = {
      Description = "Lock screen when going to sleep/suspend";
      Before = [
        "sleep.target"
        "suspend.target"
      ];
    };

    Install = {
      WantedBy = [
        "sleep.target"
        "suspend.target"
      ];
    };

    Service = {
      Type = "Type"
      Environment = [
               "DISPLAY=:0"
      ];
      ExecStart = "${pkgs.betterlockscreen}/bin/dropbox --lock";
      TimeoutSec = "infinity";
      ExecStartPost = "sleep 1"
    };
  };
}
