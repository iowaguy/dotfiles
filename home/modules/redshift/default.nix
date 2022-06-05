{
    services.redshift = {
      enable = true;
      provider = "geoclue2";
      temperature.day = 6500;
      temperature.night = 3000;
      tray = true;
      settings = {
        redshift.brightness-day = 1.0;
        redshift.brightness-night = 0.25;
      };
    };
}
