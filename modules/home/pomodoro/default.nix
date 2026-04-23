{ config, lib, pkgs, ... }:

{
  home.file = {
    # Hostnames sinkholed only during Pomodoro *work* phases when pomodoroFocusDns is enabled.
    ".config/pomodoro/blocklist.txt" = {
      force = true;
      text = ''
        # Bare hostnames or /etc/hosts-style (leading IPv4 ignored), e.g. nytimes.com or 127.0.0.1 nytimes.com
        # Lines starting with # are ignored.
        127.0.0.1 fastmail.com
        127.0.0.1 outlook.cloud.microsoft
        127.0.0.1 office.outlook.com
        127.0.0.1 scotusblog.com
        127.0.0.1 nytimes.com
      '';
    };
    ".bin/pomodoro" = {
      source = ./pomodoro.sh;
      executable = true;
    };
    ".bin/beep.wav" = {
      source = ./beep.wav;
      executable = false;
    };
    ".bin/pomodoro-control" = {
      source = ./pomodoro-control.sh;
      executable = true;
    };
  };
}
