{ self, config, lib, pkgs, ... }:
{
  services = {
    nginx.virtualHosts = {
      ${config.services.nextcloud.hostName} = {
        forceSSL = true;
        enableACME = true;
        extraConfig = ''
          add_header Content-Security-Policy "default-src 'self';";
        '';
      };

      ${config.services.nextcloud.onlyoffice.hostName} = {
        forceSSL = true;
        enableACME = true;
      };
    };

    nextcloud = {
      enable = true;
      hostName = "email.ben-weintraub.com";

       # Need to manually increment with every major upgrade.
      package = pkgs.nextcloud31;

      # Let NixOS install and configure the database automatically.
      database.createLocally = true;

      # Let NixOS install and configure Redis caching automatically.
      configureRedis = true;

      # Increase the maximum file upload size to avoid problems uploading videos.
      maxUploadSize = "16G";
      https = true;

      autoUpdateApps.enable = true;
      extraAppsEnable = true;
      extraApps = with config.services.nextcloud.package.packages.apps; {
        # List of apps we want to install and are already packaged in
        # https://github.com/NixOS/nixpkgs/blob/master/pkgs/servers/nextcloud/packages/nextcloud-apps.json
        inherit calendar contacts mail notes onlyoffice tasks;

        # Custom app installation example.
        # cookbook = pkgs.fetchNextcloudApp rec {
        #   url =
        #     "https://github.com/nextcloud/cookbook/releases/download/v0.10.2/Cookbook-0.10.2.tar.gz";
        #   sha256 = "sha256-H7KVeISBnu0/4Q31ihhiXvRtkXz4yLGOAsAj5ERgeCM=";
        #   license = "gpl3";
        # };
      };

      settings = {
        overwriteprotocol = "https";
        default_phone_region = "ET";
      };

      config = {
        dbtype = "pgsql";
        adminuser = "admin";
        adminpassFile = "./nextcloud-admin-pass";
      };
    };

    onlyoffice = {
      enable = true;
      hostname = "nextcloud.ben-weintraub.com";
    };
  };
}
