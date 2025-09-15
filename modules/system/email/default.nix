{ self, config, lib, pkgs, ... }:
{
  services = {
    nginx.virtualHosts = {
      ${config.services.nextcloud.hostName} = {
        forceSSL = true;
        enableACME = true;
      };

      ${config.services.onlyoffice.hostname} = {
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
        dbhost = "/run/postgresql";
        adminuser = "admin";
        adminpassFile = "./nextcloud-admin-pass";
      };

      # for redis caching
      extraOptions = {
        redis = {
          host = "127.0.0.1";
          port = 31638;
          dbindex = 0;
          timeout = 1.5;
        };
      };
    };

    # for redis caching
    redis.servers.nextcloud = {
      enable = true;
      port = 31638;
      bind = "127.0.0.1";
    };

    onlyoffice = {
      enable = true;
      hostname = "nextcloud.ben-weintraub.com";
    };

    postgresql = {
      enable = true;
      ensureDatabases = [ "nextcloud" ];
      ensureUsers = [{
        name = "nextcloud";
        ensurePermissions."DATABASE nextcloud" = "ALL PRIVILEGES";
      }];
    };

    # optional backup for postgresql db
    postgresqlBackup = {
      enable = true;
      location = "/data/backup/nextclouddb";
      databases = [ "nextcloud" ];
      # time to start backup in systemd.time format
      startAt = "*-*-* 23:15:00";
    };
  };

  # ensure postgresql db is started with nextcloud
  systemd = {
    services."nextcloud-setup" = {
      requires = [ "postgresql.service" ];
      after = [ "postgresql.service" ];
    };
  };
}
