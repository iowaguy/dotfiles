{ config, pkgs, ... }:

{
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

    # Use Cloudflare and Quad9 recursive resolvers
    nameservers = [ "1.1.1.1" "9.9.9.9" ];
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
    # Used in frontrunning project. Enable when needed.
    mongodb = {
      # enable = true;
      enable = false;
      # package = pkgs.mongodb-5_0;
    };

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
      xkb.options = "altwin:swap_lalt_lwin"; # Swap left alt with left win
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

  # Needed for impermanence lib
  programs.fuse.userAllowOther = true;

  # Needed for bluetooth
  programs.dconf.enable = true;
}
