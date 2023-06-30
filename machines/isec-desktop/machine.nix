{ config, pkgs, ... }:

let passCmd = entry: "${pkgs.pass}/bin/pass ${entry} 2> /dev/null";
in {
  boot = {
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
    hostName = "isec-desktop"; # Define your hostname.

    # NOTE this must be unique among machines (and has to be 32-bit)
    hostId = "00000002";

    # Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    interfaces.enp0s31f6.useDHCP = true;
  };

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
    snowflake-proxy.enable = true;

    xserver.dpi = 220;
  };
}
