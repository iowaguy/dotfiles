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
    git-sync = {
      enable = true;
      repositories = {
        "password-store" = {
          interval = 600; # sync every hour
          path = "${config.home.homeDirectory}/.password-store";
          uri = "git@github.com:iowaguy/passwords.git";
        };
      };
    };
  };
}
