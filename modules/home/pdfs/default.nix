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

  xdg.mimeApps = {
    enable = true;
    # Make zathura the default pdf reader
    defaultApplications = {
      "application/pdf" = [ "org.pwmt.zathura.desktop" ];
      "image/pdf" = [ "org.pwmt.zathura.desktop" ];
    };
  };
}
