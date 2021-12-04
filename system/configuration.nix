# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs ? import (import ./nix/sources.nix).nixpkgs {}, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./machine/current.nix
  ] ++ (import ./wm);

  networking = {
    # The global useDHCP flag is deprecated, therefore explicitly set to
    # false here.
    useDHCP = false;

    # Block sites that distract me
    extraHosts = ''
      127.0.0.1 nytimes.com
      127.0.0.1 news.ycombinator.com
    '';
    firewall.allowedTCPPorts = [
      22067 # syncthing
      36885 # syncthing
    ];
    firewall.allowedUDPPorts = [
      36885 # syncthing
    ];
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.ben = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHA2+s16j8CHT54sw3eenPv48zg1gHzSabsRhkEt87Ss ben@weintraub.xyz"
    ];
    isNormalUser = true;
    extraGroups = [
      "wheel" # Enable 'sudo' for the user.
      "networkmanager" # Allow user to change network settings
      "docker"
      "libvirtd"
      "power"
      "syncthing"
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
    which
    busybox
    zfs
    emacs

    # TODO remove if emacs is okay. Not sure why I needed this
    # (emacsWithPackages (epkgs: [ epkgs.orgPackages.org-plus-contrib ]))
    xorg.xrandr # display manager (X Resize and Rotate protocol)
  ];

  programs = {
    # TODO: move to home.nix when rycee/home-manager#1087 resolved
    # https://github.com/rycee/home-manager/issues/1087
    ssh.startAgent = true;

    zsh = {
      enable = true;
      ohMyZsh = {
        enable = true;
        plugins = [
          "colored-man-pages"
          "git-auto-fetch"
          "vagrant"
        ];
        theme = "agnoster";
      };
      # Completion for many CLI utils
      enableBashCompletion = true;
    };

    gnupg.agent = {
      enable = true;
      pinentryFlavor = "gtk2";
    };
  };

  nixpkgs.config.allowUnfree = true;

  # Nix daemon config
  nix = {
    # Automate `nix-store --optimise`
    autoOptimiseStore = true;

    # Automate garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    # TODO remove when flakes are included by default
    package = pkgs.nixFlakes;

    extraOptions = ''
      # TODO remove when flakes are included by default
      experimental-features = nix-command flakes

      # Avoid unwanted garbage collection when using nix-direnv
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

  fonts.fonts = with pkgs; [ powerline-fonts font-awesome ];

  services = {
    blueman.enable = true;
    emacs.defaultEditor = true;

    # Used by redshift. Gets my current location.
    geoclue2.enable = true;

    # make capslock := ctrl
    # make ctrl+alt+backspace kill the X server
    xserver.xkbOptions = "ctrl:nocaps,terminate:ctrl_alt_bksp";

    syncthing = {
      enable = true;
      user = "ben";
      declarative = {
        folders = {
          "/home/ben/workspace" = {
            id = "workspace";
            devices = [
              "ben-isec"
              "kansas"
            ];
            rescanInterval = 300;
          };
        };
        devices = {
          kansas = {
            id = "MDCFDOP-O7O42LB-2NKDRRR-FLQ2SD7-GUZZ2HU-KIVUCZR-M2KVH2T-BQ4XRAW";
          };
          ben-isec = {
            id = "KOUHRCO-OYWBPGP-WRC3IEB-NHWXAFT-5JPHA3L-NRRRLMR-Y74AHAJ-QZKMUA2";
          };
        };
      };
    };
  };

  krb5 = {
    enable = true;
    # forwardable tickets necessary to do things like access AFS without
    # re-`kinit`-ing (you want this)
    libdefaults = { forwardable = true; };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system = {
    stateVersion = "20.09"; # Did you read the comment?
    autoUpgrade = {
      enable = true;
      allowReboot = false;
    };
  };
}
