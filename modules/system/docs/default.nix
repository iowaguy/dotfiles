{
  services = {
    nginx = {
      virtualHosts."docs.ben-weintraub.com" = {
        enableACME = true;
        forceSSL = true;
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
        '';
      };
      virtualHosts."sandbox-docs.ben-weintraub.com" = {
        forceSSL = true;
        enableACME = true;
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
        '';
      };
    };

    cryptpad = {
      enable = true;
      configureNginx = true;
      settings = {
        maxUploadSize = 1099511627776; # 1TiB
        httpAddress = "0.0.0.0";
        httpUnsafeOrigin = "https://docs.ben-weintraub.com";
        httpSafeOrigin = "https://sandbox-docs.ben-weintraub.com";
        addminKeys = [
          "[ben@docs.ben-weintraub.com/9XhLMbg4esIUUtEd5HabfFhkEuu6ySyl-lzBQrxYVkw=]"
        ];
      };
    };
  };
}
