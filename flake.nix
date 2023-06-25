{
  description = "My NixOS configuration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    impermanence.url = "github:nix-community/impermanence";

    home-manager = {
      url = "github:nix-community/home-manager/release-22.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ self, nixpkgs, impermanence, home-manager }:
    let
      pkgs = import nixpkgs { config = { allowUnfree = true; }; };
    in
    {
    nixosConfigurations = {
      x1-2021 = nixpkgs.lib.nixosSystem {
        specialArgs = { inherit inputs; };
        system = "x86_64-linux";
        modules = [
          ./system/configuration.nix
          home-manager.nixosModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.ben = import ./home/home.nix {inherit pkgs; inherit impermanence;};
          }
        ];
      };
    };
  };
}
