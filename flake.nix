{
  description = "My NixOS configuration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    impermanence.url = "github:nix-community/impermanence";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, impermanence, home-manager, nur }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
    };
    pkgsUnstable = import nixpkgs-unstable {
      inherit system;
      config.allowUnfree = true;

      # TODO try removing this on next update (written 12/11/23)
      config.permittedInsecurePackages = [
        "electron-25.9.0"
      ];
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
            home-manager.users.ben = import ./home.nix {inherit pkgs pkgsUnstable inputs;};
          }
        ];
      };
      isec-desktop = nixpkgs.lib.nixosSystem {
        modules = [
          ./machines/isec-desktop
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.ben = import ./home.nix {inherit pkgs pkgsUnstable inputs;};
          }
        ];
      };
    };
  };
}
