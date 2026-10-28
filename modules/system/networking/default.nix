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
    networkmanager = {
        enable = true;
        wifi.macAddress = "stable-ssid";
        # logLevel = "DEBUG";
    };

    # Use Cloudflare and Quad9 recursive resolvers
    # nameservers = [ "1.1.1.1" "9.9.9.9" ];
    #
  };
  environment.etc."NetworkManager/system-connections/flour-cambridgeport.nmconnection" = {
    mode = "0600";
    text = ''
        [connection]
        id=flour-cambridgeport
        type=wifi

        [wifi]
        ssid=Flour Bakery + Cafe Guest
        cloned-mac-address=permanent

        [ipv4]
        method=auto

        [ipv6]
        method=disabled
    '';
  };
}
