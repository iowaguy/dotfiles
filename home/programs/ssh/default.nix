{ pkgs, config, ... }:

let
  dag = config.lib.dag;
in {
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "achtung" = {
        extraOptions = {
          "ForwardAgent" = "yes";
          "Hostname" = "achtung.ccs.neu.edu";
          "User" = "bweintraub";
        };
      };
      "dome" = dag.entryAfter [ "achtung" ] {
        extraOptions = {
          "Hostname" = "192.168.2.55";
          "ProxyJump" = "achtung";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
        };
      };
      "hood" = dag.entryAfter [ "achtung" ] {
        extraOptions = {
          "Hostname" = "192.168.2.56";
          "ProxyJump" = "achtung";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
        };
      };
      "zion*" = dag.entryAfter [ "achtung" ] {
        extraOptions = {
          "ProxyJump" = "achtung";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
        };
      };
      "khoury-login" = {
        extraOptions = {
          "Hostname" = "login.khoury.northeastern.edu";
          "User" = "benweintraub";
        };
      };
      "khoury" = dag.entryAfter [ "khoury-login" ] {
        extraOptions = {
          "Hostname" = "vdi-linux-030.ccs.neu.edu";
          "ProxyJump" = "khoury-login";
          "User" = "benweintraub";
          "ForwardAgent" = "yes";
        };
      };
      "jump.csail.mit.edu" = {
        extraOptions = {
          "GSSAPIAuthentication" = "yes";
          "VerifyHostKeyDNS" = "yes";
        };
      };
      "*.csail.mit.edu !jump.csail.mit.edu 128.52.* 128.30.* 128.31.* !128.31.26.*" =
        dag.entryAfter [ "jump.csail.mit.edu" ] {
          extraOptions = {
            "ProxyJump" = "blw@jump.csail.mit.edu";
            "GSSAPIAuthentication" = "yes";
            "GSSAPIDelegateCredentials" = "yes";
          };
        };
    };
  };
}
