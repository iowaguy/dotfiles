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
          "Port" = "22";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
        };
      };
      "zion01" = {
        extraOptions = {
          "Hostname" = "zion01";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion02" = {
        extraOptions = {
          "Hostname" = "zion02";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion03" = {
        extraOptions = {
          "Hostname" = "zion03";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion04" = {
        extraOptions = {
          "Hostname" = "zion04";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion05" = {
        extraOptions = {
          "Hostname" = "zion05";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion06" = {
        extraOptions = {
          "Hostname" = "zion06";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion07" = {
        extraOptions = {
          "Hostname" = "zion07";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion08" = {
        extraOptions = {
          "Hostname" = "zion08";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion09" = {
        extraOptions = {
          "Hostname" = "zion09";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
      "zion10" = {
        extraOptions = {
          "Hostname" = "zion10";
          "User" = "bweintraub";
          "ForwardAgent" = "yes";
          "ProxyCommand" = "ssh achtung -W %h:%p";
        };
      };
    };
  };
}
