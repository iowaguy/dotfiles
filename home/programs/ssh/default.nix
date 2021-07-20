{ pkgs, config, ... }:

let
  dag = config.lib.dag;
in {
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "achtung" = {
        extraOptions = {
          "Hostname" = "achtung.ccs.neu.edu";
          "User" = "bweintraub";
        };
      };
      "zion*" = dag.entryAfter [ "achtung" ] {
        extraOptions = {
          "ProxyJump" = "achtung";
          "User" = "bweintraub";
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
