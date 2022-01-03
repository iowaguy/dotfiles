{ pkgs, home, ... }:

{
  programs.rofi = {
    enable = true;
    theme = "fancy";
    terminal = "${pkgs.kitty}/bin/kitty";
    pass = {
      enable = true;
      stores = ["${builtins.getEnv "HOME"}/.password-store"];
      extraConfig = ''
        URL_field='url'
        USERNAME_field='login'
        AUTOTYPE_field='autotype'
      '';
    };

    plugins = with pkgs; [
      rofi-calc
      rofi-file-browser
    ];
  };
}