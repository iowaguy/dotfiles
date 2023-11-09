{ config, lib, pkgs, ... }:
{
  systemd.user = {
    services = {
      stand-up-timer = {
        Install = {
          WantedBy = ["default.target"];
        };
        Service = {
          BusName = "org.weintraub.StandUpTimer";
          Restart = "on-failure";
          ExecStart = "${pkgs.dunst}/bin/dunstify --urgency=low 'Stand Up and Drink Water!'";
        };
        Unit = {
          Description = "Stand up timer";
        };
      };
    };
    timers = {
      stand-up-timer = {
        Unit = {
          Description = "Stand up timer";
        };
        Timer = {
          OnUnitActiveSec = "30min"; # go off every 30min after the first run
        };
      };
    };
  };
}
