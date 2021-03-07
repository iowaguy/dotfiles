{ config, lib, pkgs, ... }:

{
  programs.emacs = {
    extraPackages = epkgs: [
      epkgs.use-package
      pkgs.ispell
    ];
    enable = true;
  };

  services.emacs = {
    enable = true;
  };

  home.links.".emacs.d" = "./workspace/dotfiles/emacs/dot-emacs.d";

  home.file.".bin/em" = {
    text = ''
      #!/bin/sh
      emacsclient -nc $@
    '';
    executable = true;
  };

  home.file.".signature".text = ''
    Ben Weintraub
    PhD Student
    Khoury College of Computer Sciences
    Northeastern University
    https://ben-weintraub.com/
  '';

  xdg.dataFile."applications/emacsclient.desktop".text = ''
    [Desktop Entry]
    Name=EmacsClient
    GenericName=Text Editor
    Comment=Edit text
    MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
    Exec=.bin/em %F
    Icon=emacs
    Type=Application
    Terminal=false
    Categories=Development;TextEditor;
    StartupWMClass=Emacs
    Keywords=Text;Editor;
  '';
}
