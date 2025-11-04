{ pkgs, config, ... }:

let
  dag = config.lib.dag;
in {
  services.ssh-agent.enable = true;
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "*" = {
        setEnv = { TERM = "xterm-256color"; };
        serverAliveInterval = 240;
      };
      "glacier" = {
        hostname = "glacier";
        user = "ben";
      };
      "pi.hole" = {
        hostname = "192.168.0.37";
        user = "ben";
      };
      "acadia" = {
        hostname = "acadia-wg-tunnel";
        user = "ben";
      };
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
      "jump.csail.mit.edu" = {
        extraOptions = {
          "VerifyHostKeyDNS" = "yes";
        };
      };
      "*.csail.mit.edu !jump.csail.mit.edu 128.52.* 128.30.* 128.31.* !128.31.26.*" =
        dag.entryAfter [ "jump.csail.mit.edu" ] {
          proxyJump = "blw@jump.csail.mit.edu";
      };
    };
  };
}
