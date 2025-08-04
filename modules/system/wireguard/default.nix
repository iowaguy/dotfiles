{
  networking = {
    firewall.allowedUDPPorts = [ 51820 ];
    wireguard = {
      enable = true;
      interfaces = {
        wg0 = {
          ips = [ "10.100.0.2/24" ];
          privateKeyFile = "/home/ben/wireguard-keys/private";
          listenPort = 51820; # to match firewall allowedUDPPorts (without this wg uses random port numbers)
          peers = [
            {
              name = "bens-laptop";
              publicKey = "2POqMTQ7ozHbAbGo06h7clIqAEHnMOndCfOz0WSdm2k=";
              endpoint = "129.10.186.241:51820";
              allowedIPs = [ "10.100.0.0/24" ];
              persistentKeepalive = 25;
              # presharedKeyFile = "/home/ben/wireguard-keys/presharedkey.psk";
            }
          ];
        };
      };
    };
  };
}
