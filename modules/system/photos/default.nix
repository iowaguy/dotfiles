{ config, ... }:
{
  services = {
    immich = {
      enable = true;
      machine-learning.enable = false;
    };

    nginx = {
      virtualHosts."photos.ben-weintraub.com" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
            proxyPass = "http://[::1]:${toString config.services.immich.port}";
            proxyWebsockets = true;
            recommendedProxySettings = true;
            extraConfig = ''
              client_max_body_size 50000M;
              proxy_read_timeout   600s;
              proxy_send_timeout   600s;
              send_timeout         600s;
            '';
          };
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
        '';
      };
    };
  };
}
