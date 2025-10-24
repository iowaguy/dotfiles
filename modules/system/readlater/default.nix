{ config, ... }:
{
  services = {
    nginx = {
      virtualHosts."links.ben-weintraub.com" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
            proxyPass = "http://[::1]:${toString config.services.karakeep.extraEnvironment.PORT}";
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

    karakeep = {
     enable = true;
      extraEnvironment = {
        # PORT = "1234";
        DISABLE_SIGNUPS = "true";
        DISABLE_NEW_RELEASE_CHECK = "true";
      };
    };
  };
}
