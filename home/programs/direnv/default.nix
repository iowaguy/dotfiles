{ config, lib, pkgs, ... }:

{
    # When I switch to a directory, I want it to assume a certain
    # environment.
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv = {
      enable = true;
      # optional for nix flakes support
      enableFlakes = true;
    };
  };

  home.file.".bin/use_nix" = {
    text = ''
      #!/bin/sh
      cp ${builtins.getEnv "HOME"}/.bin/shell.nix.template ./shell.nix
      echo "use nix" >> .envrc
    '';
    executable = true;
  };

  home.file.".bin/shell.nix.template" = {
    source = ./shell.nix.template;
  };

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
