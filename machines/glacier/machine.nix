{ pkgs, config, ... }: {

  environment = {
    systemPackages = with pkgs; [
      wget
      vim
      git
      which
      busybox
      emacs
      htop-vim
      bat # A better cat
      ripgrep # A better grep
    ];
  };


  programs = {
    mosh = {
      enable = true;
      openFirewall = true;
    };
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
        vim = "nvim";
        vi = vim;
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

  location.provider = "geoclue2";

  virtualisation = {
    docker.enable = true;
    libvirtd.enable = true;
  };

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
  system.stateVersion = "23.11";
}
