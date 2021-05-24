{ config, pkgs, ... }:

{

  boot = {
    loader = {
      # Use the GRUB 2 boot loader.
      grub.enable = true;
      grub.version = 2;
      # Define on which hard drive you want to install Grub.
      grub.device = "/dev/sda"; # or "nodev" for efi only
    }
  cleanTmpDir = true;

  networking = {
    hostName = "kansas"; # Define your hostname.

    # Some desktop environments use NetworkManager for configuring
    # networking.
    networkmanager.enable = true;

    # Per-interface useDHCP will be mandatory in the future, so this
    # generated config replicates the default behaviour.
    interfaces.wlp4s0.useDHCP = true;
  };

  sound.enable = true;
  hardware = {
    # opengl = {
    #   # Apparently this is currently only supported by ati_unfree drivers, not ati
    #   driSupport32Bit = false;
    #   extraPackages = [ pkgs.vaapiIntel pkgs.vaapiVdpau ];
    # };
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
    autorandr.enable = true;

    xserver = {
      videoDrivers = [ "ati" ];

      # keyboard settings
      xkbOptions = "ctrl:nocaps"; # make capslock := ctrl

      xrandrHeads = [
        {
          output = "eDP";
          primary = true;
        }
        {
          output = "HDMI-0";
          monitorConfig = ''
            Option "PreferredMode" "3840x2160"
          '';
        }
      ];
      resolutions = [
        { x = 2048; y = 1152; }
        { x = 1920; y = 1080; }
        { x = 2560; y = 1440; }
        { x = 3072; y = 1728; }
        { x = 3840; y = 2160; }
      ];
    };

    # power savings
    upower.enable = true;
    tlp.enable = true;
    thermald.enable = true; # So it doesn't get too toasty
  };

  powerManagement.powertop.enable = true;

  # systemd.services.bluetooth = {
  #   enable = true;
  #   description = "Bluetooth service";
  #   unitConfig = {
  #     Description = "Bluetooth service";
  #     ConditionPathIsDirectory = /sys/class/bluetooth;
  #     ControllerMode = "dual";
  #     Enable = "Source,Sink,Media,Socket";
  #   };
  #   serviceConfig = {
  #     Type = "dbus";
  #     BusName = "org.bluez";
  #     ExecStart = ["" "${pkgs.bluezFull}/bin/bluetoothd --noplugin=avrcp"];
  #     NotifyAccess = "main";
  #   };
  #   wantedBy = [ "multi-user.target" ];

  # };
}
