# Pomodoro focus DNS: extra sinkholes during work phases only (see design spec).
{ config, lib, pkgs, ... }:

let
  cfg = config.services.pomodoroFocusDns;
  focusConf = "/var/lib/dnsmasq/pomodoro-focus.conf";

  # Bake the blocklist path into the helper; wrap PATH so coreutils + systemd are found.
  substituted = pkgs.substituteAll {
    src = ./pomodoro-dns-apply.sh;
    blocklistPath = cfg.blocklistPath;
  };

  helper = pkgs.runCommand "pomodoro-dns-apply" {
    nativeBuildInputs = [ pkgs.makeWrapper ];
  } ''
    install -Dm755 ${substituted} $out/bin/pomodoro-dns-apply
    wrapProgram $out/bin/pomodoro-dns-apply \
      --prefix PATH : ${lib.makeBinPath (with pkgs; [ bash coreutils systemd ])}
  '';
in
{
  options.services.pomodoroFocusDns = {
    enable = lib.mkEnableOption "Pomodoro work-phase extra DNS blocking via dnsmasq";
    blocklistPath = lib.mkOption {
      type = lib.types.str;
      default = "/home/ben/.config/pomodoro/blocklist.txt";
      description = "Absolute path to the user-editable hostname list.";
    };
  };

  config = lib.mkIf cfg.enable {
    services.dnsmasq = {
      enable = true;
      resolveLocalQueries = true;
      settings = {
        server = [ "1.1.1.1" "9.9.9.9" ];
        conf-file = [ focusConf ];
      };
    };

    environment.systemPackages = [ helper ];

    systemd.tmpfiles.rules = [
      "f /var/lib/dnsmasq/pomodoro-focus.conf 0644 root root -"
    ];

    security.sudo.extraRules = [
      {
        users = [ "ben" ];
        commands = [
          {
            command = "${helper}/bin/pomodoro-dns-apply";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];
  };
}
