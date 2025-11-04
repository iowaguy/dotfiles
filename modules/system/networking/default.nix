{
  networking = {
    # The global useDHCP flag is deprecated, therefore explicitly set to
    # false here.
    useDHCP = false;

    # Block sites that distract me
    extraHosts = builtins.readFile ./hosts.txt;

    firewall.enable = true;

    # Some desktop environments use NetworkManager for configuring
    # networking.
    networkmanager.enable = true;

    # Use Cloudflare and Quad9 recursive resolvers
    # nameservers = [ "1.1.1.1" "9.9.9.9" ];
  };
}
