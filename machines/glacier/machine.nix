{ pkgs, config, ... }: {
  imports = [
    ./hardware-configuration.nix
  ];

  time.timeZone = "America/New_York";

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  environment = {
    systemPackages = with pkgs; [
      wget
      vim
      git
      firefox
      which
      busybox
      emacs
      htop-vim
      bat # A better cat
      ripgrep # A better grep
      ente
    ];
  };


  # Members of wheel don't need a password for sudo
  security.sudo.wheelNeedsPassword = false;
  security.acme = {
    defaults.email = "ben+acme@weintraub.xyz";
    acceptTerms = true;
    # certs = {
    #   "docs.ben-weintraub.com" = {
    #     webroot = "/var/lib/acme";
    #   };
    # };
  };

  programs = {
    zoxide.enableZshIntegration = true;
    direnv.enableZshIntegration = true;
    neovim.enable = true;

    # TODO: move to home.nix when rycee/home-manager#1087 resolved
    # https://github.com/rycee/home-manager/issues/1087
    ssh.startAgent = true;

    zsh = {
      enable = true;
      autosuggestions.enable = true;
      syntaxHighlighting.enable = true;

      shellAliases = rec {
        lla = "ls -la";
        la = lla;
        cat = "bat";
        grep = "rg";
        xclip = "xclip -selection clipboard";
      };
      ohMyZsh = {
        enable = true;
        theme = "robbyrussell";
      };
    };

    gnupg.agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gtk2;
    };

  };


  services = {
    gnome.gnome-keyring.enable = true;
    openssh.enable = true; # Enable the OpenSSH daemon.

    # Enable this when I want to debug
    netdata.enable = false;


    nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
      virtualHosts."docs.ben-weintraub.com" = {
        enableACME = true;
        forceSSL = true;
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
        '';
      };
      virtualHosts."sandbox-docs.ben-weintraub.com" = {
        forceSSL = true;
        enableACME = true;
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains; preload" always;
        '';
      };
    };

    cryptpad = {
      enable = true;
      configureNginx = true;
      settings = {
        httpAddress = "0.0.0.0";
        httpUnsafeOrigin = "https://docs.ben-weintraub.com";
        httpSafeOrigin = "https://sandbox-docs.ben-weintraub.com";
        addminKeys = [
          "[ben@docs.ben-weintraub.com/9XhLMbg4esIUUtEd5HabfFhkEuu6ySyl-lzBQrxYVkw=]"
        ];
      };
    };
  };

  users.users = {
    ben = {
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHA2+s16j8CHT54sw3eenPv48zg1gHzSabsRhkEt87Ss ben@weintraub.xyz"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGVxtjrPG1RJTXrI+7ftjQLfqgwL2GXDfAGyaVA1Qaaa ben@x1-2021"
      ];
      isNormalUser = true;
      extraGroups = [
        "wheel" # Enable 'sudo' for the user.
        "networkmanager" # Allow user to change network settings
        "docker"
        "qemu-libvirtd"
        "libvirtd"
        "power"
        "syncthing"
      ];
      shell = pkgs.zsh;
    };

    root.openssh.authorizedKeys.keys = [''ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGVxtjrPG1RJTXrI+7ftjQLfqgwL2GXDfAGyaVA1Qaaa ben@x1-2021'' ];
  };


  boot.tmp.cleanOnBoot = true;
  zramSwap.enable = false;
  networking = {
    hostName = "glacier";
    domain = "";
    firewall.allowedTCPPorts = [
      80
      443
    ];
  };

  # Nix daemon config
  nix = {
    # Automate garbage collection
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    # TODO remove when flakes are included by default
    package = pkgs.nixVersions.stable;

    extraOptions = ''
      # TODO remove when flakes are included by default
      experimental-features = nix-command flakes

      # Avoid unwanted garbage collection when using nix-direnv
      keep-outputs     = true
      keep-derivations = true
    '';

    settings = {
      # Required by Cachix to be used as non-root user
      trusted-users = [ "root" "ben" ];

      # Automate `nix-store --optimise`
      auto-optimise-store = true;
    };
  };

  location.provider = "geoclue2";

  virtualisation = {
    docker.enable = true;
    # docker.storageDriver = "zfs";
    libvirtd.enable = true;
  };

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
  system.stateVersion = "23.11";
}
