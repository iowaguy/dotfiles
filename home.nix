attrs@{ pkgs, pkgsUnstable, inputs, ... }:

let
  defaultPkgs = with pkgs; [
    arandr                        # A GUI for autorandr
    cabal-install
    cachix                        # cache binaries so I don't have to rebuild
    caffeine-ng                   # don't fall asleep when I have fullscreen vids playing
    curl
    dolphin                       # file explorer
    drawio                        # good for drawing finite state machines
    entr
    et                            # A convenient cli timer
    fd
    feh
    file                          # classic tool for viewing file attributes
    graphviz
    (haskellPackages.ghcWithPackages (hpkgs: [
      hpkgs.xmobar
      hpkgs.xmonad
      hpkgs.xmonad-contrib
    ])) # ghci with packages needed for debugging xmonad
    jq
    jetbrains.idea-community
    libsForQt5.konqueror          # Needed to open links in the browser when I click on them
    libnotify
    libreoffice                   # A horrible program I have to use sometimes to view MS office docs
    mongodb-compass               # A GUI for MongoDB
    niv
    networkmanager-openvpn        # a nice GUI interface for openVPN in NetworkManager
    networkmanagerapplet
    ngrok                         # HTTP and TCP tunneling service
    nox
    openvpn                       # a VPN client
    ormolu                        # Haskell linter/formatter
    pinentry-gtk2
    pavucontrol                   # pulseaudio volume control
    paprefs                       # pulseaudio preferences
    pasystray                     # pulseaudio systray
    ripgrep
    rubber                        # a nice tool for compiling latex
    skypeforlinux
    sqlite
    stack                         # Haskell build tool
    texlive.combined.scheme-full
    tree
    vlc
    wireshark
    xclip
    xmobar
    zoom-us                       # Video conferencing
  ];

  # These are packages that I want to keep up-to-date
  unstablePkgs = with pkgsUnstable; [
    discord                       # A chat client
    docker-compose
    notion-app-enhanced
    obsidian                      # Notes
    signal-desktop
    spotify                       # Musics
    zotero                        # Citation manager
  ];

in {
  imports = with inputs; [
    impermanence.nixosModules.home-manager.impermanence
    nur.nixosModules.nur
    (import ./modules/home/email (attrs // {inherit pkgsUnstable;}))
    (import ./modules/home/brave (attrs // {inherit pkgsUnstable;}))
  ] ++ (import ./modules/home) ++ (import ./code);

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = rec {
    username = "ben";
    homeDirectory = "/home/${username}";

    packages = defaultPkgs ++ unstablePkgs;

    file.".background-image".source = ./resources/background-image;
    persistence."/persist/${homeDirectory}" = {
        directories = [
          "workspace"
          "Zotero"
          ".mozilla"
          ".emacs.d"
          ".gnupg"
          ".ssh"
          ".config/Signal"
          ".config/skypeforlinux"
          ".config/Slack"
          ".config/syncthing"
          ".local/share/direnv"
          ".local/share/keyrings"
          ".local/share/tridactyl"
          ".packer.d"
          ".password-store"
          ".terraform.d"
          ".vagrant.d"
        ];
        allowOther = true;
      };
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

    # for converting files between types
    pandoc.enable = true;

    # A better ls
    exa.enable = true;

    # Interactive fuzzy searching from the command line
    fzf = {
      enable = true;
    };

    # The z command for jumping around --- a better cd
    zoxide = {
      enable = true;
    };

    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    # A nice tool for viewing processes and system stats
    htop.enable = true;

    gpg.enable = true;
    bat.enable = true;
    broot.enable = true;
  };

  services = {
    # Send alerts for battery events
    poweralertd.enable = true;

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