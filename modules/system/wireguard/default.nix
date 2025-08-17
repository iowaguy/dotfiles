{
  networking = {
    wg-quick = {
      interfaces = {
        wg0 = {
          address = [ "192.168.100.2/32" ];
          privateKeyFile = "/home/ben/wireguard-keys/private";
          autostart = true;
          peers = [
            {
              publicKey = "2POqMTQ7ozHbAbGo06h7clIqAEHnMOndCfOz0WSdm2k=";
              endpoint = "acadia.ben-weintraub.com:51820";
              allowedIPs = [ "192.168.100.0/24" ];
              persistentKeepalive = 25;
              presharedKeyFile = "/home/ben/wireguard-keys/presharedkey.psk";
            }
          ];
        };
      };
    };
  };
}
