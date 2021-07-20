{ pkgs, home, ... }:

{
  programs.rofi = {
    enable = true;
    theme = "fancy";
    terminal = "${pkgs.alacritty}/bin/alacritty";
    pass = {
      enable = true;
      stores = ["${builtins.getEnv "HOME"}/.password-store"];
      extraConfig = ''
        URL_field='url'
        USERNAME_field='user'
        AUTOTYPE_field='autotype'
      '';
    };

    # TODO I should be able to configure rofi-calc this way after the next HM
    # release. Also enable in i3-config.ini (search for calc)
    # plugins = [
    #   pkgs.rofi-calc
    # ];
  };
}
