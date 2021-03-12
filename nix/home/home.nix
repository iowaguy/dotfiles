{ config, lib, pkgs, ... }:

with lib;

let
  defaultPkgs = with pkgs; [
    afew # initial tagging for notmuch
    alacritty
    cabal-install
    curl
    dmenu
    direnv
    dropbox-cli
    emacs26Packages.virtualenv
    entr
    evince
    fd
    ghc
    graphviz
    keybase
    libnotify
    lorri
    niv
    nixfmt
    notmuch # an email search engine
    nox
    pass
    pandoc
    pinentry-gtk2
    qnotero
    qt5Full  # needed for matplotlib
    ripgrep
    scrot
    signal-desktop
    skype
    slack
    sqlite
    stack
    teams
    texlive.combined.scheme-full
    tree
    xclip
    zoom-us
    zotero

    # Fonts
    inconsolata
  ];

  i3Pkgs = with pkgs; [
    networkmanager_dmenu   # networkmanager on dmenu
    networkmanagerapplet   # networkmanager applet
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

    packages = defaultPkgs ++ i3Pkgs;

    file.".background_image".source = ./resources/background_image;

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
      defaultApplications = {
        "application/pdf" = "evince.desktop";
        "x-scheme-handler/msteams" = "teams.desktop";
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

    zsh = {
      enable = true;
      sessionVariables = {
        BROWSER="firefox"; # toggle with lynx for headless servers
        PATH = "$HOME/.bin:$PATH";
      };
    };
  };

  services = {
    lorri.enable = true;
    udiskie.enable = true;
  };
}
