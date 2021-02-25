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
    ripgrep
    signal-desktop
    skype
    slack
    sqlite
    teams
    texlive.combined.scheme-full
    tree
    zathura
    zoom-us
    zotero

    # Fonts
    inconsolata
  ];

  polybarPkgs = with pkgs; [
    font-awesome-ttf      # awesome fonts
    material-design-icons # fonts with glyphs
  ];

  xmonadPkgs = with pkgs; [
    networkmanager_dmenu   # networkmanager on dmenu
    networkmanagerapplet   # networkmanager applet
    nitrogen               # wallpaper manager
    xcape                  # keymaps modifier
    xorg.xkbcomp           # keymaps modifier
    xorg.xmodmap           # keymaps modifier
    xorg.xrandr            # display manager (X Resize and Rotate protocol)
  ];

in {
  imports = [
    ./modules/make-links.nix
    ./programs/alacritty.nix
    ./programs/dropbox.nix
    ./programs/emacs
    ./programs/firefox.nix
    ./programs/git.nix
    ./programs/mailcap.nix
    ./programs/redshift.nix
    # ./programs/rofi/default.nix
    ./programs/slack.nix
    ./programs/ssh.nix
    ./programs/teams.nix
    # ./programs/xmonad/default.nix
    ./programs/zsh.nix
    # ./services/polybar/default.nix
    ./programs/i3/default.nix
  ];

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

    packages = defaultPkgs ++ polybarPkgs ++ xmonadPkgs;

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
        "application/pdf" = "zathura.desktop";
        "x-scheme-handler/msteams" = "teams.desktop";
      };
    };
  };

  fonts.fontconfig.enable = true;

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
  };

  services = {
    lorri.enable = true;
  };
}
