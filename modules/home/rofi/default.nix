{ pkgs, rofi-theme, ... }:

let
  customRofiTheme = pkgs.writeText "custom-theme.rasi" ''
    @import "${rofi-theme}/themes/rounded-nord-dark.rasi"

    * {
        font: "monospace 24";
    }

    #sidebar {
        border: 0px;
        background-color: inherit;
        padding: 5px;
    }
    window {
        width:            960;
        background-color: var(bg0);
        location:         north;
        y-offset:         calc( ( 50.0000%  - 176px  ));
        border-radius:    24px ;
    }

    mode-switcher {
        border:  0px;
        spacing: 0px;
        expand: false;
    }

    button {
        padding:      2px;
        border:       0px 0px 2px ;
    }
    button selected.normal {
        text-color: white;
        background-color: black/50%;

        border:       2px 2px 0px ;
        border-radius:    10px 10px 0 0;
    }
  '';
in
{
  programs.rofi = {
    enable = true;
    theme = "${customRofiTheme}";
    terminal = "${pkgs.kitty}/bin/kitty";
    extraConfig = {
      modes = "drun,ssh,calc,file-browser-extended";
      show-icons = true;
      display-drun = "Run";
      display-calc = "Calc";
      display-ssh = "SSH";
      display-file-browser-extended = "Files";
      sidebar-mode = true;
      monitor = -1; # show rofi on the monitor presently in focus
    };
    pass = {
      enable = true;
      stores = [".password-store"];
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
