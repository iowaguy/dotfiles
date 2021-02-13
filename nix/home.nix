{ config, lib, pkgs, ... }:

with lib;

let dag = config.lib.dag;
    username = "ben";
in {
  imports = [
    ./../emacs
    ./modules/make-links.nix
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
    ispell

    # Fonts
    inconsolata
  ];


  programs.git = {
    enable = true;
    ignores = [ "*~" "*.swp" ];
    userEmail = "benweintraub34@gmail.com";
    userName = "Ben Weintraub";
    extraConfig.pull.rebase = "true";
    extraConfig.github.user = "iowaguy";
  };

  programs.emacs = {
    extraPackages = epkgs: [
      epkgs.use-package
      pkgs.ispell
    ];
    enable = true;
  };

  home.file."bin/em" = {
    text = ''
      #!/bin/sh
      emacsclient -nc $@
    '';
    executable = true;
  };

  home.links.".emacs.d" = "./workspace/dotfiles/emacs/dot-emacs.d";
  home.links.".opt/sensible-defualts.el" = "./workspace/dotfiles/sensible-defaults.el";

  systemd.user.services.dropbox = {
    Unit = { Description = "Dropbox"; };

    Install = { WantedBy = [ "graphical-session.target" ]; };

    Service = {
      Environment = [
        "QT_PLUGIN_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtPluginPrefix}"
        "QML2_IMPORT_PATH=/run/current-system/sw/${pkgs.qt5.qtbase.qtQmlPrefix}"
      ];
      ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
      ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
      KillMode = "control-group"; # upstream recommends process
      Restart = "on-failure";
      PrivateTmp = true;
      ProtectSystem = "full";
      Nice = 10;
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
