{ config, lib, pkgs, ... }:
{
  # the quintessential linux password manager
  programs.password-store = {
    enable = true;
    settings = {
      PASSWORD_STORE_DIR = "${config.home.homeDirectory}/.password-store";
    };
  };

  services = {
    # things can now query `pass` for my credentials
    pass-secret-service.enable = true;
    password-store-sync = {
      enable = true;
      frequency = "*:0"; # sync every hour
    };
  };
}
