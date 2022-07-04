{ config, lib, pkgs ? import (import ./nix/sources.nix).home-manager {}, ... }:

with lib;

let
  sources = import ../nix/sources.nix;
  unstable = import sources.nixos-unstable { config.allowUnfree = true; };
  defaultPkgs = with pkgs; [
    cabal-install
    cachix                        # cache binaries so I don't have to rebuild
    caffeine-ng                   # don't fall asleep when I have fullscreen vids playing
    curl
    discord                       # A chat client
    drawio                        # good for drawing finite state machines
    entr
    et                            # A convenient cli timer
    fd
    feh
    file                          # classic tool for viewing file attributes
    ghc
    graphviz
    jq
    keybase-gui
    libnotify
    libreoffice                   # A horrible program I have to use sometimes to view MS office docs
    mongodb-compass               # A GUI for MongoDB
    niv
    networkmanager-openvpn        # a nice GUI interface for openVPN in NetworkManager
    ngrok                         # HTTP and TCP tunneling service
    nox
    openvpn                       # a VPN client
    pandoc
    pinentry-gtk2
    pavucontrol                   # pulseaudio volume control
    paprefs                       # pulseaudio preferences
    pasystray                     # pulseaudio systray
    qt5Full                       # needed for matplotlib
    ripgrep
    rubber                        # a nice tool for compiling latex
    signal-desktop
    skypeforlinux
    spotify                       # Musics
    sqlite
    stack                         # Haskell build tool

    taffybar                      # A cool status bar
    haskellPackages.gtk-sni-tray         # gtk-sni-tray-standalone, needed for taffybar https://github.com/taffybar/taffybar/issues/355
    haskellPackages.status-notifier-item # status-notifier-watcher for, needed for taffybar https://github.com/taffybar/taffybar/issues/355

    texlive.combined.scheme-full
    tree
    vivaldi
    vlc
    wireshark
    xclip
    zoom-us                       # Video conferencing
  ];

  # These are packages that I want to keep up-to-date
  unstablePkgs = with unstable; [
    zotero                        # Citation manager
  ];

in {
  imports = [
    ./machine/current.nix
  ] ++ (import ./modules) ++ (import ./code);

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = p: {
      nur = import (builtins.fetchTarball sources.nur.url) { inherit pkgs; };
    };
  };

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = {
    username = "ben";
    homeDirectory = "/home/ben";

    packages = defaultPkgs ++ unstablePkgs;

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

  # Store user configurations in .config directory
  xdg = {
    enable = true;

    # Let me define what programs should open what files
    mimeApps.enable = true;
  };

  programs = {
    # A better ls
    exa.enable = true;

    # Interactive fuzzy searching from the command line
    fzf = {
      enable = true;
      enableZshIntegration = true;
    };

    # The z command for jumping around --- a better cd
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    # A nice tool for viewing processes and system stats
    htop.enable = true;

    gpg.enable = true;

    bat.enable = true;

    broot = {
      enable = true;
      enableZshIntegration = true;
    };
  };

  services = {
    udiskie.enable = true;

    # For screenshots
    flameshot.enable = true;
    screen-locker = {
      enable = true;
      lockCmd = "${pkgs.betterlockscreen}/bin/betterlockscreen --lock";
      inactiveInterval = 20;
    };
  };
}
