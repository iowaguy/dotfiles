let
  apis = [
    "eth"
    "net"
    "web3"
    "admin"
    "debug"
    "personal"
    "txpool"
  ];
in {
  services.geth = {
    "debug" = {
      enable = true;
      syncmode = "light";
      maxpeers = 100;
      port = 30303; # default port, just making explicit
      http = {
        enable = true;
        apis = apis;
        address = "0.0.0.0";
        port = 8545; # default port, just making explicit
      };
      websocket = {
        enable = true;
        apis = apis;
        address = "0.0.0.0";
        port = 8546; # default port, just making explicit
      };
      extraArgs = [
        "console"
        "--txpool.pricebump=1"
        "--txpool.globalslots=250000"
        "--txpool.globalqueue=50000"
        "--http.vhosts=*"
      ];
    };
    "nu-lux-analysis" = {
      enable = true;
      syncmode = "light";
      maxpeers = 100;
      port = 30303; # default port, just making explicit
      http = {
        enable = true;
        apis = apis;
        address = "0.0.0.0";
        port = 8545; # default port, just making explicit
      };
      websocket = {
        enable = true;
        apis = apis;
        address = "0.0.0.0";
        port = 8546; # default port, just making explicit
      };
      extraArgs = [
        "--txpool.pricebump=1"
        "--txpool.globalslots=250000"
        "--txpool.globalqueue=50000"
        "--http.vhosts=*"
      ];
    };
  };
}
