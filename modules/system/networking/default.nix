{
  networking = {
    # The global useDHCP flag is deprecated, therefore explicitly set to
    # false here.
    useDHCP = false;

    # Block sites that distract me
    extraHosts = builtins.readFile ./hosts.txt;

    firewall.allowedTCPPorts = [
      22067 # syncthing
      36885 # syncthing
    ];
    firewall.allowedUDPPorts = [
      36885 # syncthing
    ];

    # Some desktop environments use NetworkManager for configuring
    # networking.
    networkmanager.enable = true;

    # Use Cloudflare and Quad9 recursive resolvers
    nameservers = [ "1.1.1.1" "9.9.9.9" ];
  };
}
