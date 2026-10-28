{ pkgs, config, ... }:

let
  dag = config.lib.dag;
in {
  services.ssh-agent.enable = true;
  programs.ssh = {
    enableDefaultConfig = false;
    enable = true;
    settings = {
      "*" = {
        setEnv = ''
            TERM=xterm-256color
        '';
        serverAliveInterval = 240;
      };
      "pi.hole" = {
        hostname = "pi.hole";
        user = "ben";
      };
      "acadia" = {
        hostname = "acadia";
        user = "ben";
      };
      "ngrok-acadia" = {
        hostname = "0.tcp.eu.ngrok.io";
        user = "ben";
        port = 19673;
      };
      # "acadia" = {
      #   hostname = "acadia-wg-tunnel";
      #   user = "ben";
      # };
      "achtung" = {
        forwardAgent = true;
        hostname = "achtung.ccs.neu.edu";
        user = "bweintraub";
      };
      "dome" = dag.entryAfter [ "achtung" ] {
        hostname = "192.168.2.55";
        proxyJump = "achtung";
        user = "bweintraub";
        forwardAgent = true;
      };
      "hood" = dag.entryAfter [ "achtung" ] {
        hostname = "192.168.2.56";
        proxyJump = "achtung";
        user = "bweintraub";
        forwardAgent = true;
      };
      "zion*" = dag.entryAfter [ "achtung" ] {
        proxyJump = "achtung";
        user = "bweintraub";
        forwardAgent = true;
      };
      "honeypot" = dag.entryAfter [ "zion*" ] {
        port = 2222;
        proxyJump = "zion01";
        user = "honeypot";
        forwardAgent = true;
        hostname = "localhost";
      };
      "khoury-login" = {
        hostname = "login.khoury.northeastern.edu";
        user = "benweintraub";
      };
    };
  };
}
