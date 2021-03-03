{ config, lib, pkgs, ... }:

with lib;

let
  defaultPkgs = with pkgs; [
    alacritty
    curl
    dmenu
    direnv
    dropbox-cli
    emacs26Packages.virtualenv
    entr
    fd
    graphviz
    keybase
    libnotify
    lorri
    niv
    nixfmt
    nox
    pass
    pinentry-gtk2
    qnotero
    qt5Full  # needed for matplotlib
    ripgrep
    scrot
    signal-desktop
    skype
    slack
    sqlite
    teams
    texlive.combined.scheme-full
    tree
    xclip
    zathura
    zoom-us
    zotero

    # Fonts
    inconsolata


    (stdenv.mkDerivation {
      name = "notmuch";
      src = fetchgit {
        url = "git://git.notmuchmail.org/git/notmuch";
        rev = "0.31.4";
        sha256 = "04q9zwy6mpck82zk70xnx2knh2jmqhf676703kjw0fbvdrzw9qik";
      };
      installPhase = ''
        mkdir -p $out
        # cp airpods $out/bin
        # chmod u+x $out/bin/airpods
      '';
      buildInputs = [
        cppcheck
        emacs
        glib
        gmime3
        gnupg
        gpgme
        perl
        pkg-config
        python38
        talloc
        xapian
        zlib
      ];
    })
  ];

  i3Pkgs = with pkgs; [
    networkmanager_dmenu   # networkmanager on dmenu
    networkmanagerapplet   # networkmanager applet
    xorg.xrandr # display manager (X Resize and Rotate protocol)
  ];

    # i3blocks-airpods


in {
  imports = (import ./programs) ++ (import ./modules);

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
        "application/pdf" = "org.pwmt.zathura.desktop";
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
  };
}
