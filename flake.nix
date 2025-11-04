{
  description = "My NixOS configuration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    impermanence.url = "github:nix-community/impermanence";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rofi-theme = {
      url = "github:newmanls/rofi-themes-collection";
      flake = false;
    };

  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, impermanence, home-manager, nur , rofi-theme}:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [ nur.overlays.default ];
    };
    pkgsUnstable = import nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;
    };
  in {
    nixosConfigurations = {
      x1-2021 = nixpkgs.lib.nixosSystem {
        modules = [
          ./machines/x1-2021
          ./modules/system/geoclue2
          ./modules/system/kerberos
          ./modules/system/networking
          ./modules/system/nix
          ./modules/system/persist
          ./modules/system/printing
          ./modules/system/screen-lock
          ./modules/system/security
          ./modules/system/syncthing
          ./modules/system/time
          ./modules/system/users
          ./modules/system/wireguard
          ./modules/system/wm/xmonad
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.ben = import ./home.nix {inherit pkgs pkgsUnstable inputs rofi-theme;};
          }
        ];
      };
      glacier = nixpkgs.lib.nixosSystem {
        modules = [
          ./machines/glacier
          ./modules/system/docs
          ./modules/system/email
          ./modules/system/networking
          ./modules/system/nix
          ./modules/system/photos
          ./modules/system/readlater
          ./modules/system/security
          ./modules/system/time
          ./modules/system/users
        ];
      };
    };
  };
}
