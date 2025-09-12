{ config, pkgs, ... }:

{
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
      xsecurelock
    ];

    # get completion for system packages (e.g. systemd).
    pathsToLink = [ "/share/zsh" ];
  };

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
        wpa_supplicant = pkgs.wpa_supplicant.overrideAttrs (attrs: {
          patches = attrs.patches ++ [ ../../resources/nuwave.patch ];
        });
      };
  };

  boot = {
    kernel.sysctl = {
      "kernel.sysrq" = 255;
    };
    # NOTE force downgrade kernel because of screen flickering bug in 6.1. Try new kernel after 23.11 release.
    kernelPackages = pkgs.linuxPackages_5_15;

    supportedFilesystems = [ "zfs" ];
    loader = {
      grub = {
        enable = true;
        efiSupport = true;
        zfsSupport = true;
        device = "nodev";
        efiInstallAsRemovable = true;
      };
    };
    tmp.cleanOnBoot = true;
    crashDump.enable = true;
  };

  networking = {
    hostName = "x1-2021"; # Define your hostname.

    # NOTE this must be unique among machines (and has to be 32-bit)
    hostId = "00000003";

    # Some desktop environments use NetworkManager for configuring
    # networking.
    networkmanager.enable = true;

    # Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    interfaces.wlp0s20f3.useDHCP = true;
  };

  hardware = {
    graphics = {
      # Apparently this is currently only supported by ati_unfree drivers, not ati
      enable32Bit = false;
      extraPackages = with pkgs; [
        vaapiIntel
        vaapiVdpau
      ];
    };
    cpu.intel.updateMicrocode = true;

    bluetooth = {
      enable = true;
    };
  };

  services = {
    gnome.gnome-keyring.enable = true;
    openssh.enable = true; # Enable the OpenSSH daemon.

    # Enable handling of hotplug and sleep events by autorandr
    autorandr.enable = true;

    # Enable this when I want to debug
    netdata.enable = false;
    blueman.enable = true;
    emacs.defaultEditor = true;

    libinput = {
      # one, two, three fingered clicks (on touchpad) map to left, right, middle clicks
      touchpad.clickMethod = "clickfinger";
    };

    displayManager.defaultSession = "plasmax11";
    # displayManager.defaultSession = "xfce+i3";
    # displayManager.defaultSession = "xfce+xmonad";

    xserver = {
      dpi = 220;
      displayManager = {
        sessionCommands = ''
          # Suspends and locks session on lid close
          xset s 600
          xset dpms 600 600 600
        '';
      };

      # keyboard settings
      xkb.options = "altwin:swap_lalt_lwin,ctrl:nocaps,terminate:ctrl_alt_bksp"; # Swap left alt with left win
    };

    # power savings
    upower.enable = true;
    thermald.enable = true; # So it doesn't get too toasty

    # Audio stuff
    pipewire = {
      enable = true;
      audio.enable = true;
      pulse.enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      jack.enable = true;
    };
  };

  powerManagement.powertop.enable = true;

  programs = {
    # Needed for impermanence lib
    fuse.userAllowOther = true;

    # Needed for bluetooth
    dconf.enable = true;
    zsh.enable = true;

    gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gtk2;
    };

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

  fonts.packages = with pkgs; [
    powerline-fonts
    font-awesome
  ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

  virtualisation = {
    docker.enable = true;
    docker.storageDriver = "zfs";
    libvirtd.enable = true;
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
