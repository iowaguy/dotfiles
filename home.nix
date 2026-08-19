attrs@{ pkgs, pkgsUnstable, inputs, ... }:

let
  defaultPkgs = with pkgs; [
    texlive.combined.scheme-full  # Don't want to rebuild frequently b/c it's big
  ];

  # These are packages that I want to keep up-to-date
  unstablePkgs = with pkgsUnstable; [
    arandr                        # A GUI for autorandr
    caffeine-ng                   # don't fall asleep when I have fullscreen vids playing
    claude-code
    code-cursor                   # AI IDE
    curl
    cursor-cli
    delve                         # Golang debugger
    dig
    discord                       # A chat client
    docker-compose
    drawio                        # Good for drawing finite state machines
    feh
    gnumake                       # 'make' command
    jdk17
    jetbrains.idea                # IntelliJ IDEA Java IDE
    karere                        # Whatsapp for Linux
    mongodb-compass               # A GUI for MongoDB
    mosh                          # A connectionless ssh replacement
    mtr                           # MyTraceroute. A nice network debugging tool
    neovim
    obsidian                      # Notes
    pavucontrol                   # pulseaudio volume control
    paprefs                       # pulseaudio preferences
    pasystray                     # pulseaudio systray
    python313Packages.tqdm
    rubber                        # a nice tool for compiling latex
    signal-desktop
    spotify                       # Musics
    telegram-desktop
    tcpdump                       # Network debugging tool
    tree
    vlc
    ytdownloader                  # Download youtube videos
    zoom-us                       # Video conferencing
    zotero                        # Citation manager
    cabal-install
    cachix                        # cache binaries so I don't have to rebuild
    entr
    et                            # A convenient cli timer
    fastfetch                     # Pretty print system stats
    file                          # classic tool for viewing file attributes
    graphviz
    (haskellPackages.ghcWithPackages (hpkgs: [
      hpkgs.xmobar
      hpkgs.xmonad
      hpkgs.xmonad-contrib
      hpkgs.xmonad-extras
    ])) # ghci with packages needed for debugging xmonad
    hwatch                        # A modern alternative to the watch command
    jq
    libnotify
    libreoffice                   # A horrible program I have to use sometimes to view MS office docs
    libxcb-cursor
    networkmanager-openvpn        # a nice GUI interface for openVPN in NetworkManager
    networkmanagerapplet
    openvpn                       # a VPN client
    ormolu                        # Haskell linter/formatter
    pinentry-gtk2
    sqlite
    stack                         # Haskell build tool
    wireshark
    wireguard-tools
    xclip
    xdg-utils
    xmobar
    zip                           # Zip files
  ];

in {
  imports = [
    ./modules/home/email
    ./modules/home/brave
    ./modules/home/rofi
    ./modules/home/firefox
    ./modules/home
    ./code
  ];
  _module.args = inputs // attrs;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = {
    username = "ben";

    packages = defaultPkgs ++ unstablePkgs;

    file.".background-image".source = ./resources/background-image;
    persistence."/persist" = {
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

    # Enable xdg portal
    portal = {
      enable = true;
      extraPortals = [ pkgs.kdePackages.xdg-desktop-portal-kde ];
      xdgOpenUsePortal = true;
      config.common.default = "kde";
    };
  };

  programs = {
    vscode = {
      enable = true;
      package = pkgsUnstable.vscode;
      profiles.default.extensions = with pkgs.vscode-extensions; [
        vscodevim.vim
        redhat.java
        ms-python.python
        github.copilot
        ms-toolsai.jupyter
        ms-toolsai.vscode-jupyter-cell-tags
        ms-toolsai.jupyter-renderers
        ms-python.python
        bbenoist.nix
        jnoortheen.nix-ide
      ];
    };

    # for converting files between types
    pandoc.enable = true;

    # A better ls
    eza.enable = true;

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
    htop = {
      enable = true;
      package = pkgs.htop-vim; # allows vim keybindings in htop
    };

    gpg.enable = true;
    bat.enable = true;
    broot.enable = true;
  };

  services = {
    # Tray app that prevents the system from sleeping
    # when enabled
    caffeine.enable = true;

    # Send alerts for battery events
    poweralertd.enable = true;

    udiskie.enable = true;

    # For screenshots
    flameshot.enable = true;
  };
}
