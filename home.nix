attrs@{ pkgs, pkgsUnstable, pkgs2405, inputs, rofi-theme, ... }:

let
  defaultPkgs = with pkgs; [
    alacritty                     # A terminal b/c sometimes kitty is incompatible
    arandr                        # A GUI for autorandr
    cabal-install
    cachix                        # cache binaries so I don't have to rebuild
    caffeine-ng                   # don't fall asleep when I have fullscreen vids playing
    curl
    dig
    kdePackages.dolphin           # file explorer
    drawio                        # good for drawing finite state machines
    entr
    et                            # A convenient cli timer
    fastfetch                     # Pretty print system stats
    fd
    feh
    file                          # classic tool for viewing file attributes
    graphviz
    (haskellPackages.ghcWithPackages (hpkgs: [
      hpkgs.xmobar
      hpkgs.xmonad
      hpkgs.xmonad-contrib
      hpkgs.xmonad-extras
    ])) # ghci with packages needed for debugging xmonad
    jq
    libsForQt5.konqueror          # Needed to open links in the browser when I click on them
    kdePackages.kdenlive
    libsForQt5.kwallet            # VS Code wants this
    libnotify
    libreoffice                   # A horrible program I have to use sometimes to view MS office docs
    mongodb-compass               # A GUI for MongoDB
    mosh                          # A connectionless ssh replacement
    mtr                           # MyTraceroute. A nice network debugging tool
    networkmanager-openvpn        # a nice GUI interface for openVPN in NetworkManager
    networkmanagerapplet
    ngrok                         # HTTP and TCP tunneling service
    neovim
    openvpn                       # a VPN client
    ormolu                        # Haskell linter/formatter
    pinentry-gtk2
    pavucontrol                   # pulseaudio volume control
    paprefs                       # pulseaudio preferences
    pasystray                     # pulseaudio systray
    ripgrep
    rubber                        # a nice tool for compiling latex
    # skypeforlinux
    sqlite
    stack                         # Haskell build tool
    texlive.combined.scheme-full
    python312Packages.tqdm
    python312Packages.git-filter-repo
    tcpdump                       # Network debugging tool
    tree
    vlc
    wireshark
    wireguard-tools
    xclip
    xmobar
    zip                           # Zip files
    zoom-us                       # Video conferencing
    zotero                        # Citation manager
  ];

  # These are packages that I want to keep up-to-date
  unstablePkgs = with pkgsUnstable; [
    discord                       # A chat client
    docker-compose
    gnumake                       # 'make' command
    # jetbrains.idea-community-bin  # IntelliJ IDEA Java IDE
    jetbrains.idea-ultimate       # IntelliJ IDEA Java IDE
    obsidian                      # Notes
    signal-desktop
    spotify                       # Musics
    ytdownloader                  # Download youtube videos
    jdk17
  ];

in {
  imports = with inputs; [
    impermanence.nixosModules.home-manager.impermanence
    # nur.modules.homeManager.default
    (import ./modules/home/email (attrs // {inherit pkgsUnstable;}))
    (import ./modules/home/brave (attrs // {inherit pkgs2405;}))
    (import ./modules/home/rofi (attrs // {inherit rofi-theme;}))
    (import ./modules/home/firefox (attrs // {inherit nur;}))
    # (import ./modules/home/jupyter (attrs // {inherit pkgsUnstable;}))
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
      ] ++
      [
        pkgsUnstable.vscode-extensions.github.copilot-chat
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
  };
}
