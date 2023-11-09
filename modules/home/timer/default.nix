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
          ExecStart = "${pkgs.dunst}/bin/dunstify --urgency=low 'Stand Up!'";
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
          OnCalendar = "Mon..Fri";
          OnActiveSec = "30min";
        };

      };
    };
  };
}
