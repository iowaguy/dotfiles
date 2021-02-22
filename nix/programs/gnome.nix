{ pkgs, ... }:

{
  home.file.".gtkrc-2.0".text = ''
    gtk-key-theme-name = "Emacs"
    gtk-font-name="Oxygen-Sans Sans-Book 13"
  '';
}
