{ pkgs, config, lib, ... }: {

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

  nixpkgs.config = {
    allowUnfree = true;
  };

  services = {
    gnome.gnome-keyring.enable = true;
    openssh = {
      enable = true; # Enable the OpenSSH daemon.
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = "no";
        AllowUsers = [ "ben" ];
      };
    };

    # Enable this when I want to debug
    netdata.enable = false;

    nginx = {
      enable = true;
      recommendedProxySettings = true;
      recommendedTlsSettings = true;
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

  networking.networkmanager.enable = lib.mkForce true;

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;
  system.stateVersion = "23.11";
}
