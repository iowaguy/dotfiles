{
  description = "My NixOS configuration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    nixpkgs-24-05.url = "github:NixOS/nixpkgs/nixos-24.05"; # needed for brave
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

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, impermanence, home-manager, nur, nixpkgs-24-05 , rofi-theme}:
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
    pkgs2405 = import nixpkgs-24-05 {
      inherit system;
      config.allowUnfree = true;
    };
  in {
    nixosConfigurations = {
      x1-2021 = nixpkgs.lib.nixosSystem {
        modules = [
          ./machines/x1-2021
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.ben = import ./home.nix {inherit pkgs pkgsUnstable pkgs2405 inputs rofi-theme;};
          }
        ];
      };
      glacier = nixpkgs.lib.nixosSystem {
        modules = [
          ./machines/glacier
        ];
      };
    };
  };
}
