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
    crashDump.enable = true;
  };

  networking = {
    hostName = "x1-2021"; # Define your hostname.

    # NOTE this must be unique among machines (and has to be 32-bit)
    hostId = "00000003";

    # Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    interfaces.wlp0s20f3.useDHCP = true;
  };

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
    upower.enable = true; # power savings
    thermald.enable = true; # So it doesn't get too toasty
  };

  powerManagement.powertop.enable = true;

  # Needed for impermanence lib
  programs.fuse.userAllowOther = true;
}
