{ config, lib, pkgs, ... }:

with lib;

let
  defaultPkgs = with pkgs; [
    cabal-install
    cachix                       # cache binaries so I don't have to rebuild
    caffeine-ng                  # don't fall asleep when I have fullscreen vids playing
    curl
    direnv
    entr
    evince
    fd
    ghc
    graphviz
    jq
    keybase-gui
    libnotify
    lorri
    niv
    nixfmt
    nox
    pass
    pandoc
    pinentry-gtk2
    pavucontrol                   # pulseaudio volume control
    paprefs                       # pulseaudio preferences
    pasystray                     # pulseaudio systray
    qnotero
    qt5Full                       # needed for matplotlib
    ripgrep
    rubber                        # a nice tool for compiling latex
    scrot                         # screenshots
    signal-desktop
    skype
    spotify                       # Musics
    sqlite
    stack                         # Haskell build tool
    texlive.combined.scheme-full
    tree
    xclip
    zoom-us
    zotero
  ];

in {
  imports = (import ./programs) ++ (import ./modules) ++ (import ./services);

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = p: {
      nur = import (import pinned/nur.nix) { inherit pkgs; };
    };
  };
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = {
    username = "ben";
    homeDirectory = "/home/ben";

    packages = defaultPkgs;

    file.".background-image".source = ./resources/background-image;

    # This value determines the Home Manager release that your
    # configuration is compatible with. This helps avoid breakage
    # when a new Home Manager release introduces backwards
    # incompatible changes.
    #
    # You can update Home Manager without changing this value. See
    # the Home Manager release notes for a list of state version
    # changes in each release.
    stateVersion = "21.03";
  };

  xdg = {
    enable = true;
    mimeApps = {
      enable = true;
      associations.added = {
        "application/pdf" = [ "evince.desktop" ];
        "x-scheme-handler/msteams" = [ "teams.desktop" ];
      };
    };
  };

  programs = {
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    htop = {
      enable = true;
      sortDescending = true;
      sortKey = "PERCENT_CPU";
    };

    # When I switch to a directory, I want it to assume a certain
    # environment.
    direnv = {
      enable = true;
      enableZshIntegration = true;
      enableNixDirenvIntegration = true;
    };

    gpg.enable = true;

    bat.enable = true;

    broot = {
      enable = true;
      enableZshIntegration = true;
    };
  };

  services = {
    lorri.enable = true;
    udiskie.enable = true;
    screen-locker = {
      enable = true;
      lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen --lock";
      inactiveInterval = 20;
    };
  };
}
