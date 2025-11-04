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
      hostName = "nextcloud.ben-weintraub.com";

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
        inherit calendar contacts mail notes
          onlyoffice tasks cookbook cospend
          forms maps news whiteboard;
      };

      settings = {
        overwriteprotocol = "https";
        default_phone_region = "ET";
      };

      config = {
        dbtype = "pgsql";
        adminuser = "admin";
        adminpassFile = "/home/ben/dotfiles/modules/system/email/nextcloud-admin-pass";
      };
    };

    onlyoffice = {
      enable = true;
      hostname = "office.ben-weintraub.com";
    };

    postgresql = {
      enable = true;
      ensureDatabases = [ "nextcloud" ];
      ensureUsers = [{name = "nextcloud";}];
    };

    # optional backup for postgresql db
    postgresqlBackup = {
      enable = true;
      location = "/data/backup/nextclouddb";
      databases = [ "nextcloud" ];
      # time to start backup in systemd.time format
      startAt = "*-*-* 23:15:00";
    };

    roundcube = {
      enable = true;
      hostName = "email.ben-weintraub.com";
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
