{ config, pkgs, ... }:

{
  swapDevices = [
    {
      device = "/dev/sda4";
      randomEncryption.enable = true;
    }
  ];
  boot = {
    supportedFilesystems = [ "zfs" ];
    zfs.devNodes = "/dev";
    loader = {
      generationsDir.copyKernels = true;
      grub = {
        enable = true;
        # Use the GRUB 2 boot loader.
        version = 2;

        copyKernels = true;
        efiSupport = true;
        zfsSupport = true;
        # for systemd-autofs
        extraPrepareConfig = ''
          mkdir -p /boot/efis
          for i in /boot/efis*; do mount $i ; done
        '';
        ## For problematic UEFI hardware
        # efiInstallAsRemovable = true;
        # Define on which hard drive you want to install Grub.
        devices = [ "/dev/sda" ]; # or "nodev" for efi only
        mirroredBoots = [
          {
            devices = [ "/dev/sda" ];
            efiSysMountPoint = "/boot/efis/sda1";
            path = "/boot";
          }
        ];
      };
      efi = {
        ## if UEFI firmware can detect entries, set to true
        canTouchEfiVariables = false;
        efiSysMountPoint = "/boot/efis/sda1";
      };
    };
    cleanTmpDir = true;
  };

  networking = {
    hostName = "isec-desktop"; # Define your hostname.

    # Some desktop environments use NetworkManager for configuring
    # networking.
    networkmanager.enable = true;

    # Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    interfaces.enp0s31f6.useDHCP = true;
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

    };

    # power savings
    upower.enable = true;
    tlp.enable = true;
    thermald.enable = true; # So it doesn't get too toasty
  };
}
