{
  description = "My NixOS configuration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
    nur.url = "github:nix-community/NUR";
    impermanence.url = "github:nix-community/impermanence";

    home-manager = {
      url = "github:nix-community/home-manager/release-23.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, impermanence, home-manager, nur }:
    let
      pkgs = import nixpkgs {
        config.allowUnfree = true;
      };
      pkgsUnstable = import nixpkgs-unstable {
        config.allowUnfree = true;
      };
    in {
    nixosConfigurations = {
      x1-2021 = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        system = "x86_64-linux";
        modules = [
          ./machines/x1-2021
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.ben = import ./home.nix {inherit pkgs; inherit pkgsUnstable; inherit impermanence; inherit nur;};
          }
        ];
      };
      isec-desktop = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        system = "x86_64-linux";
        modules = [
          ./machines/isec-desktop
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.ben = import ./home.nix {inherit pkgs; inherit pkgsUnstable; inherit impermanence; inherit nur;};
          }
        ];
      };
    };
  };
}
