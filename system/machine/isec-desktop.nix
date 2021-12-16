{ config, pkgs, ... }:

{
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
    hostName = "isec-desktop"; # Define your hostname.

    # NOTE this must be unique among machines (and has to be 32-bit)
    hostId = "00000002";

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

    pulseaudio.enable = true;
  };

  services = {
    # Enable handling of hotplug and sleep events by autorandr
    #autorandr.enable = true;

    xserver = {
#      videoDrivers = [ "ati" ];
      displayManager = {
        defaultSession = "xfce+i3";
      };

      # keyboard settings
      xkbOptions = "altwin:swap_lalt_lwin"; # Swap left alt with left win
    };
  };
}
