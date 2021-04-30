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
  };
}
