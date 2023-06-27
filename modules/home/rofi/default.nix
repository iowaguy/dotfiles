{ pkgs, home, config, ... }:

{
  programs.rofi = {
    enable = true;
    theme = "fancy";
    terminal = "${pkgs.kitty}/bin/kitty";
    pass = {
      enable = true;
      stores = ["${config.home.homeDirectory}/.password-store"];
      extraConfig = ''
        URL_field='url'
        USERNAME_field='login'
        AUTOTYPE_field='autotype'
        help_color='#4872FF'
      '';
    };

    plugins = with pkgs; [
      rofi-calc
      rofi-file-browser
    ];
  };
}
