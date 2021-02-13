{ config, lib, pkgs, ... }:

with lib;

let dag = config.lib.dag;
    username = "ben";
in {
  imports = [
    ./modules/make-links.nix
    ./programs/emacs
    ./programs/dropbox.nix
    ./programs/git.nix
    ./programs/redshift.nix
    ./programs/zsh.nix
    ./programs/alacritty/alacritty.nix
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "${username}";
  home.homeDirectory = "/home/${username}";

  nixpkgs.config.allowUnfree = true;

  xdg.enable = true;

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    alacritty
    dropbox-cli
    tree
    pass
    ripgrep
    sqlite
    fd
    entr
    libnotify
    pinentry-gtk2
    keybase

    # Fonts
    inconsolata
  ];

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
