{
  networking = {
    # The global useDHCP flag is deprecated, therefore explicitly set to
    # false here.
    useDHCP = false;

    # Block sites that distract me
    extraHosts = ''
      127.0.0.1 nytimes.com
      127.0.0.1 news.ycombinator.com
    '';
    firewall.allowedTCPPorts = [
      22067 # syncthing
      36885 # syncthing
      30303 # geth
      8545 # geth
      8546 # geth
    ];
    firewall.allowedUDPPorts = [
      36885 # syncthing
      30303 # geth
      8545 # geth
      8546 # geth
    ];
  };
}
