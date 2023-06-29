{ config, pkgs, ... }:

{
  boot = {
    kernel.sysctl = {
      "kernel.sysrq" = 255;
    };
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

  sound.enable = true;
  hardware = {
    opengl = {
      # Apparently this is currently only supported by ati_unfree drivers, not ati
      driSupport32Bit = false;
      extraPackages = [ pkgs.vaapiIntel pkgs.vaapiVdpau ];
    };
    cpu.intel.updateMicrocode = true;

    pulseaudio = {
      enable = true;

      # NixOS allows either a lightweight (default) or full build of PulseAudio to be installed.
      # Only the full build has Bluetooth support, so it must be selected here.
      package = pkgs.pulseaudioFull;
      extraConfig = ''
        load-module module-switch-on-connect
      '';
    };
    bluetooth = {
      enable = true;
    };
  };

  services = {
    # Used in frontrunning project. Enable when needed.
    mongodb = {
      enable = false;
      package = pkgs.mongodb-5_0;
    };

    xserver = {
      dpi = 220;
#      videoDrivers = [ "ati" ];
      displayManager = {
        # defaultSession = "xfce+i3";
        # defaultSession = "xfce+xmonad";
        defaultSession = "plasma";
        sessionCommands = ''
          # Suspends and locks session on lid close
          xfconf-query -c xfce4-session -p /general/LockCommand -s "systemctl suspend";
        '';
      };


      # keyboard settings
      xkbOptions = "altwin:swap_lalt_lwin"; # Swap left alt with left win

      libinput = {
        # one, two, three fingered clicks (on touchpad) map to left, right, middle clicks
        touchpad.clickMethod = "clickfinger";
      };
    };

    # power savings
    upower.enable = true;
    thermald.enable = true; # So it doesn't get too toasty
  };

  powerManagement.powertop.enable = true;

  # Needed for impermanence lib
  programs.fuse.userAllowOther = true;
}
