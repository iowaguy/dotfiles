{
  # Set your time zone.
  # time.timeZone = "America/New_York";
  # time.timeZone = "CET"; # for when in Europe
  # time.timeZone = "America/Chicago";
  # time.timeZone = "America/Boise"; # for Mountain Time
  #### This should be handled automatically by `localtimed`

  # a simple daemon for keeping the system timezone
  # up-to-date based on the current location.
  services.localtimed.enable = true;


  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };
}
