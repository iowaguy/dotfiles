{ config, pkgs, ... }:

{
  boot = {
    loader = {
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
    cleanTmpDir = true;
  };
  networking = {
    hostName = "boston"; # Define your hostname.

    # Some desktop environments use NetworkManager for configuring
    # networking.
    networkmanager.enable = true;

    # Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    interfaces.wlp4s0.useDHCP = true;
  };

  # Enable sound.
  sound.enable = true;
  hardware = {
    cpu.intel.updateMicrocode = true;

    # Use the MBP camera
    facetimehd.enable = true;

    pulseaudio = {
      enable = true;

      # NixOS allows either a lightweight (default) or full build of PulseAudio to be installed.
      # Only the full build has Bluetooth support, so it must be selected here.
      package = pkgs.pulseaudioFull;
    };
    bluetooth = {
      enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    bluez
    linuxPackages.facetimehd # TODO not sure if I still need this
  ];
}
