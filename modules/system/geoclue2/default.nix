
{

  ## Turn on debugging logs in journal
  # systemd.services.geoclue.serviceConfig.Environment = [
  #   "G_MESSAGES_DEBUG=Geoclue"
  # ];

  location.provider = "geoclue2";

  # Used by redshift and localtime. Gets my current location.
  services.geoclue2 = {
    enable = true;

    # For some reason the provided wifi access points are
    # always from California. Disable so I get my actual
    # location.
    enableWifi = false;
  };
}
