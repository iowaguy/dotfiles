{
  networking = {
    firewall.allowedTCPPorts = [
      8545
      8546
      30303
    ];
    firewall.allowedUDPPorts = [
      8545
      8546
      30303
    ];
    # firewall.allowedUDPPortRanges = [
    #   {
    #     from = 30303;
    #     to = j
    #   }
    # ];
  };

  services.geth."nu-lux-analysis" = {
    enable = true;
    syncmode = "light";
    maxpeers = 200;
    websocket = {
      enable = true;
      apis = [
        "eth"
        "net"
        "web3"
        "admin"
        "debug"
        "personal"
        "txpool"
      ];
      address = "0.0.0.0";
    };
    http = {
      enable = true;
      apis = [
        "eth"
        "net"
        "web3"
        "admin"
        "debug"
        "personal"
        "txpool"
      ];
      address = "0.0.0.0";
    };
    extraArgs = [
      "--http.vhosts=*"
      "--ws.origins=*"
      "--txpool.pricebump=1"
      "--txpool.globalslots=250000"
      "--txpool.globalqueue=50000"
      "--verbosity=5"
    ];
  };
}
# /usr/bin/geth --datadir=/home/cool/chain_data/mainnet --config /home/cool/config.toml --syncmode full --gcmode archive --maxpeers 100 --cache 8192

# Here is also the content of the config.toml file:

# [Node.HTTPTimeouts]
# ReadTimeout = 9223372036854775807
# WriteTimeout = 9223372036854775807
# IdleTimeout = 9223372036854775807
