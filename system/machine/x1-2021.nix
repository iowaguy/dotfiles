{ config, pkgs, ... }:

{
  imports = [
    ../persist/system.nix
  ];

  boot = {
    supportedFilesystems = [ "zfs" ];
    loader = {
      systemd-boot.enable = true;
      grub = {
        enable = true;
        version = 2;
        efiSupport = true;
        zfsSupport = true;
        device = "nodev";
        efiInstallAsRemovable = true;
      };
    };
    cleanTmpDir = true;
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
    # Enable handling of hotplug and sleep events by autorandr
    #autorandr.enable = true;

    xserver = {
#      videoDrivers = [ "ati" ];
      displayManager = {
        defaultSession = "xfce+i3";
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
    tlp.enable = true;
    thermald.enable = true; # So it doesn't get too toasty
  };

  powerManagement.powertop.enable = true;

  # Needed for impermanence lib
  programs.fuse.userAllowOther = true;
}