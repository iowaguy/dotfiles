{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    # Used for go tools (in emacs)
    go

    # Used for spellchecking
    ispell

    # Used by "lookup"
    ripgrep
    sqlite
    wordnet

    # Typechecking in python
    nodePackages.pyright

    # Dictionaries for spelling good
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))

    # Autoformatting nix code
    nixfmt

    # Doom wants this for faster indexing
    fd

    # Needed for formatting xml
    html-tidy

    # Doom wants these for Go
    gopls
    gocode
    gotools
    gotests
    godef

    # Needed for C/C++ LSP
    ccls
    clang-tools

    # Needed for installing emacsql-sqlite for org-roam
    gcc

    # Needed for =minted= to export with syntax highlighting
    python310Packages.pygments

    # For Jupyter notebook
    python310Packages.jupyter

    python310Packages.black

    # LSP server for bash
    nodePackages.bash-language-server

    # Needed for Haskell LSP
    haskell-language-server

    python310

    # LSP impl for TeX
    texlab
  ];

  services.emacs = {
      enable = true;
#      package = pkgs.emacs29;
  };

  home.links.".doom.d" = "workspace/areas/system-management/dotfiles/modules/home/emacs/doom.d";

  home.file.".bin/em" = {
    text = ''
      #!/bin/sh
      emacsclient -nc $@ &> /dev/null
      while [[ "$?" -ne "0" ]]; do
            echo "Sleeping until Emacs server has started..."
            sleep 5
            emacsclient -nc "$@" &> /dev/null
      done
    '';
    executable = true;
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
