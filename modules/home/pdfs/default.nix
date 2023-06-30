{ pkgs, config, ... }:

{
  home.packages = with pkgs; [
    pdftk                         # A tool for working with PDFs
  ];

  # A pdf viewer
  programs.zathura = {
    enable = true;
    options = {
      selection-clipboard = "clipboard";
      recolor = true;
      recolor-reverse-video = true;
      recolor-keephue = true;
      recolor-darkcolor = "#ABB2BF";
      recolor-lightcolor = "#282C35";
    };
  };

  home.file.".local/share/applications/zathura.desktop".text = ''
    [Desktop Entry]
    Encoding=UTF-8
    Version=1.0
    Type=Application
    Terminal=false
    Exec=${config.home.profileDirectory}/bin/zathura
    Name=zathura
  '';

  xdg.mimeApps.associations.added = {
    "application/pdf" = [ "zathura.desktop" ];
    "image/pdf" = [ "zathura.desktop" ];
  };
}
