# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware/x1-2021.nix
    ./machine/x1-2021.nix
  ] ++ (import ./modules);

  # Set your time zone.
  time.timeZone = "America/New_York";
  # time.timeZone = "America/Chicago";

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
      wget
      vim
      git
      firefox
      which
      busybox
      zfs
      emacs
      xorg.xrandr # display manager (X Resize and Rotate protocol)
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
      pinentryFlavor = "gtk2";
    };

    # Lock screen before sleeping
    xss-lock.enable = true;
  };

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
        wpa_supplicant = pkgs.wpa_supplicant.overrideAttrs (attrs: {
          patches = attrs.patches ++ [ ./nuwave.patch ];
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
    package = pkgs.nixFlakes;

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
    libvirtd.enable = true;
  };

  fonts.fonts = with pkgs; [
    powerline-fonts
    font-awesome
    nerdfonts
  ];

  services = {
    printing.enable = true;

    # Needed for taffybar, to prevent error: https://github.com/NixOS/nixpkgs/issues/16327
    gnome.at-spi2-core.enable = true;

    # A Tor proxying service (helps other users find Tor bridges)
    # snowflake-proxy.enable = true;

    # Enable handling of hotplug and sleep events by autorandr
    autorandr.enable = true;

    # Enable this when I want to debug
    netdata.enable = false;
    blueman.enable = true;
    emacs.defaultEditor = true;

    # make capslock := ctrl
    # make ctrl+alt+backspace kill the X server
    xserver.xkbOptions = "ctrl:nocaps,terminate:ctrl_alt_bksp";
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
