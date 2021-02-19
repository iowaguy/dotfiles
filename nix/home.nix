{ config, lib, pkgs, ... }:

with lib;

let
  username = "ben";
in {
  imports = [
    ./modules/make-links.nix
    ./programs/alacritty/alacritty.nix
    ./programs/dropbox.nix
    ./programs/emacs
    ./programs/firefox.nix
    ./programs/git.nix
    ./programs/redshift.nix
    ./programs/signal.nix
    ./programs/skype.nix
    ./programs/slack.nix
    ./programs/ssh.nix
    ./programs/teams.nix
    ./programs/zoom.nix
    ./programs/zotero.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = p: {
      nur = import (import pinned/nur.nix) { inherit pkgs; };
    };
  };
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = {
    username = "${username}";
    homeDirectory = "/home/${username}";
  };

  # notifications about home-manager news
  news.display = "silent";


  xdg.enable = true;

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    alacritty
    dropbox-cli
    entr
    fd
    graphviz
    keybase
    libnotify
    pass
    pinentry-gtk2
    qnotero
    ripgrep
    signal-desktop
    skype
    slack
    sqlite
    teams
    tree
    zoom-us
    zotero

    # Fonts
    inconsolata
  ];

  programs = {
    htop = {
      enable = true;
      sortDescending = true;
      sortKey = "PERCENT_CPU";
    };

    # When I switch to a directory, I want it to assume a certain
    # environment.
    direnv = {
      enable = true;
      enableBashIntegration = true;
      enableNixDirenvIntegration = true;
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
