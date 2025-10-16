{
  services = {
    nginx = {
      virtualHosts."books.ben-weintraub.com" = {
        enableACME = true;
        forceSSL = true;
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
        '';
      };
    };

    calibre-server = {
      enable = true;
      auth.enable = true;
    };
  };
}
