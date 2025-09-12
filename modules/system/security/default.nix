{
  # Members of wheel don't need a password for sudo
  security.sudo.wheelNeedsPassword = false;
  security.acme = {
    defaults.email = "ben+acme@weintraub.xyz";
    acceptTerms = true;
  };
}
