{ pkgs, home, ... }:

{
  programs.rofi = {
    enable = true;
    theme = "fancy";
    terminal = "${pkgs.alacritty}/bin/alacritty";
    package = with pkgs; rofi.override { plugins = [ rofi-calc ]; };
    pass = {
      enable = true;
      stores = ["${builtins.getEnv "HOME"}/.password-store"];
      extraConfig = ''
        URL_field='url'
        USERNAME_field='login'
        AUTOTYPE_field='autotype'
      '';
    };

    # TODO I should be able to configure rofi-calc this way after the next HM
    # release.
    # plugins = [
    #   pkgs.rofi-calc
    # ];
  };
}
