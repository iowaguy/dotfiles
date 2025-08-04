{ config, lib, pkgs, ... }:

{
  home.packages = with pkgs; [
    go # Used for go tools (in emacs)
    ispell # Used for spellchecking

    # Used by "lookup"
    ripgrep
    sqlite
    wordnet

    pyright # Typechecking in python

    # Dictionaries for spelling good
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))

    nixfmt-rfc-style # Autoformatting nix code
    nil # lsp for nix
    fd # Doom wants this for faster indexing
    html-tidy # Needed for formatting xml

    # Doom wants these for Go
    gopls
    gotools
    gotests
    godef

    # Needed for C/C++ LSP
    ccls
    clang-tools

    gcc # Needed for installing emacsql-sqlite for org-roam
    glibc.dev # Include standard headers
    python312Packages.pygments # Needed for =minted= to export with syntax highlighting
    python312Packages.jupyter # For Jupyter notebook
    python312Packages.black
    nodePackages.bash-language-server # LSP server for bash
    haskell-language-server # Needed for Haskell LSP

    python312

    # LSP impl for TeX
    texlab
  ];

  services.emacs = {
      enable = true;
  };

  home.links.".doom.d" = "areas/system-management/dotfiles/modules/home/emacs/doom.d";

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
