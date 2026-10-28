{
  security = {
    # Members of wheel don't need a password for sudo
    sudo.wheelNeedsPassword = false;
    pam.services."ben".kwallet.enable = true;
    acme = {
      defaults.email = "ben+acme@weintraub.xyz";
      acceptTerms = true;
    };
  };
}
