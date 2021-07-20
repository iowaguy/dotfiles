{
    services.redshift = {
      enable = true;
      provider = "geoclue2";
      temperature.day = 5500;
      temperature.night = 3700;
      tray = true;
    };
}
