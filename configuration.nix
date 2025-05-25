# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = import ./modules/system;

  # Set your time zone.
  time.timeZone = "America/New_York";
  # time.timeZone = "CET"; # for when in Europe
  # time.timeZone = "America/Chicago";
  # time.timeZone = "America/Boise"; # for Mountain Time

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  # Members of wheel don't need a password for sudo
  security.sudo.wheelNeedsPassword = false;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = with pkgs; [
      alsa-utils
      wget
      vim
      git
      firefox
      which
      busybox
      zfs
      emacs
      xorg.xrandr # display manager (X Resize and Rotate protocol)
      libsForQt5.kscreen  # KDE display management
    ];

    # get completion for system packages (e.g. systemd).
    pathsToLink = [ "/share/zsh" ];
  };


  programs = {
    # TODO: move to home.nix when rycee/home-manager#1087 resolved
    # https://github.com/rycee/home-manager/issues/1087
    ssh.startAgent = true;

    zsh.enable = true;

    gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gtk2;
    };

    # Lock screen before sleeping
    xss-lock.enable = true;

    # Need this for VS Code to be able to run Java stuff
    nix-ld = {
      enable = true;
      libraries = with pkgs; [
        # Add any missing dynamic libraries for unpackaged programs
        # here, NOT in environment.systemPackages
        vscode-extensions.redhat.java
      ];
    };

  };

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
        wpa_supplicant = pkgs.wpa_supplicant.overrideAttrs (attrs: {
          patches = attrs.patches ++ [ ./resources/nuwave.patch ];
        });
      };
  };

  # Nix daemon config
  nix = {
    # Automate garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    # TODO remove when flakes are included by default
    package = pkgs.nixVersions.stable;

    extraOptions = ''
      # TODO remove when flakes are included by default
      experimental-features = nix-command flakes

      # Avoid unwanted garbage collection when using nix-direnv
      keep-outputs     = true
      keep-derivations = true
    '';

    settings = {
      # Required by Cachix to be used as non-root user
      trusted-users = [ "root" "ben" ];

      # Automate `nix-store --optimise`
      auto-optimise-store = true;
    };
  };

  location.provider = "geoclue2";

  virtualisation = {
    docker.enable = true;
    docker.storageDriver = "zfs";
    libvirtd.enable = true;
  };

  fonts.packages = with pkgs; [
    powerline-fonts
    font-awesome
    nerdfonts
  ];

  services = {
    gnome.gnome-keyring.enable = true;
    openssh.enable = true; # Enable the OpenSSH daemon.
    redshift.enable = true; # Save the eyes

    # Enable handling of hotplug and sleep events by autorandr
    autorandr.enable = true;

    # Enable this when I want to debug
    netdata.enable = false;
    blueman.enable = true;
    emacs.defaultEditor = true;

    xserver.xkb.options = "ctrl:nocaps,terminate:ctrl_alt_bksp";
    logind.lidSwitchDocked = "suspend";
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
      enable = false;
      allowReboot = false;
    };
  };
}
