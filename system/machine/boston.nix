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
    opengl = {
      # Apparently this is currently only supported by ati_unfree drivers, not ati
      driSupport32Bit = false;
      extraPackages = [ pkgs.vaapiIntel pkgs.vaapiVdpau ];
    };
    cpu.intel.updateMicrocode = true;

    # Use the MBP camera
    facetimehd.enable = true;

    pulseaudio = {
      enable = true;

      # NixOS allows either a lightweight (default) or full build of PulseAudio to be installed.
      # Only the full build has Bluetooth support, so it must be selected here.
      package = pkgs.pulseaudioFull;
      extraConfig = ''
        load-module module-switch-on-connect
      '';
    };
    bluetooth = { enable = true; };
  };

  services = {
    # Enable handling of hotplug and sleep events by autorandr
    autorandr.enable = true;

    # Whether to enable mbpfan, fan controller daemon for Apple Macs and MacBooks
    mbpfan.enable = true;
    xserver = {
      videoDrivers = [ "ati" ];

      displayManager = {
        defaultSession = "xfce+i3";
        sessionCommands = ''
          # Suspends and locks session on lid close
          xfconf-query -c xfce4-session -p /general/LockCommand -s "systemctl suspend";
        '';
      };

      # keyboard settings
      xkbOptions = "ctrl:nocaps"; # make capslock := ctrl

      xrandrHeads = [
        {
          output = "eDP";
          primary = true;
          # monitorConfig = ''
          #   Option "PreferredMode" "3840x2160"
          #   Option "Position" "0 0"
          # '';
        }
        {
          output = "HDMI-0";
          monitorConfig = ''
            Option "PreferredMode" "3840x2160"
          '';
        }
      ];
      resolutions = [
        {
          x = 2048;
          y = 1152;
        }
        {
          x = 1920;
          y = 1080;
        }
        {
          x = 2560;
          y = 1440;
        }
        {
          x = 3072;
          y = 1728;
        }
        {
          x = 3840;
          y = 2160;
        }
      ];
    };

    # Disable XHC1 wakeup signal to avoid resume getting triggered some time
    # after suspend. Reboot required for this to take effect.
    udev.initrdRules = ''
      SUBSYSTEM=="pci", KERNEL=="0000:00:14.0", ATTR{power/wakeup}="disabled"'';

    # power savings
    upower.enable = true;
    tlp.enable = true;
    thermald.enable = true; # So it doesn't get too toasty
  };

  powerManagement.powertop.enable = true;

  systemd.services = {
    bluetooth = {
      enable = true;
      description = "Bluetooth service";
      unitConfig = {
        Description = "Bluetooth service";
        ConditionPathIsDirectory = /sys/class/bluetooth;
        ControllerMode = "dual";
        Enable = "Source,Sink,Media,Socket";
      };
      serviceConfig = {
        Type = "dbus";
        BusName = "org.bluez";
        ExecStart = ["" "${pkgs.bluezFull}/bin/bluetoothd --noplugin=avrcp"];
        NotifyAccess = "main";
      };
      wantedBy = [ "multi-user.target" ];
    };
  };
}
