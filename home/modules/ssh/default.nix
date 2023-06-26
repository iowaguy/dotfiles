{ pkgs, config, ... }:

let
  dag = config.lib.dag;
in {
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "vagrant" = {
        extraOptions = {
          "Hostname" = "127.0.0.1";
          "User" = "vagrant";
          "Port" = "5556";
          "StrictHostKeyChecking" = "no";
          "PasswordAuthentication" = "no";
          "IdentityFile" = "/home/ben/workspace/projects/mitll/ibn/.vagrant/machines/ibn/libvirt/private_key";
          "IdentitiesOnly" = "yes";
          "LogLevel" = "FATAL";
        };
      };
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
      "netviews" = {
        extraOptions = {
          "Hostname" = "localhost";
          "User" = "ben";
          "Port" = "5556";
        };
      };
      "khoury-login" = {
        extraOptions = {
          "Hostname" = "login.khoury.northeastern.edu";
          "User" = "benweintraub";
        };
      };
      "ben-isec" = dag.entryAfter [ "khoury-login" ] {
        extraOptions = {
          "Hostname" = "ben-isec.khoury.northeastern.edu";
          "ProxyJump" = "khoury-login";
          "User" = "ben";
          "ForwardAgent" = "yes";
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
