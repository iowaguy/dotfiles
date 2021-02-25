# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix

      # Window manager
      # ./wm/xmonad.nix
      # ./wm/gnome.nix
      ./wm/i3.nix
    ];

  boot.loader = {
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot"; # ← use the same mount point here.
    };
    grub = {
      efiSupport = true;
      # Define on which hard drive you want to install Grub.
      device = "nodev";
    };
    systemd-boot.enable = true;
  };
  boot.cleanTmpDir = true;

  networking = {
    hostName = "boston"; # Define your hostname.

    # Some desktop environments use NetworkManager for configuring
    # networking.
    networkmanager.enable = true;

    # The global useDHCP flag is deprecated, therefore explicitly set to
    # false here.
    useDHCP = false;

    # Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    interfaces.wlp4s0.useDHCP = true;
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Enable sound.
  sound.enable = true;
  hardware = {
    facetimehd.enable = true;

    pulseaudio = {
      enable = true;

      # NixOS allows either a lightweight build (default) or full build of PulseAudio to be installed.
      # Only the full build has Bluetooth support, so it must be selected here.
      package = pkgs.pulseaudioFull;
    };
    bluetooth = {
      enable = true;
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ben = {
    isNormalUser = true;
    extraGroups = [
      "wheel"              # Enable 'sudo' for the user.
      "networkmanager"     # Allow user to change network settings
      "docker"
      "libvirtd"
    ];
    shell = pkgs.zsh;
  };

  # Members of wheel don't need a password for sudo
  security.sudo.wheelNeedsPassword = false;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    wget
    vim
    git
    firefox
    emacs
    bluez
    which
    linuxPackages.facetimehd
  ];

  programs = {
    # TODO: move to home.nix when rycee/home-manager#1087 resolved
    # https://github.com/rycee/home-manager/issues/1087
    ssh.startAgent = true;

    zsh.enable = true;
    zsh.ohMyZsh = {
      enable = true;
      plugins = [
        "git"
        "python"
        "man"
        "z"
        "colored-man-pages"
        "common-aliases"
        "git-auto-fetch"
        "golang"
        "httpie"
        "mvn"
        "pip"
        "vagrant"
        "web-search"
        "cabal"
      ];
      theme = "agnoster";
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          inherit pkgs;
        };
      };
    };
  };

  # Nix daemon config
  nix = {
    # Automate `nix-store --optimise`
    autoOptimiseStore = true;

    # Automate garbage collection
    gc = {
      automatic = true;
      dates     = "weekly";
      options   = "--delete-older-than 7d";
    };

    # Avoid unwanted garbage collection when using nix-direnv
    extraOptions = ''
      keep-outputs     = true
      keep-derivations = true
    '';

    # Required by Cachix to be used as non-root user
    trustedUsers = [ "root" "ben" ];
  };

  location.provider = "geoclue2";

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?
}
