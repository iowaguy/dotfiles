{
  description = "My NixOS configuration flake";

  inputs = {
    nixpkgs = {
      url = "github:NixOS/nixpkgs/nixos-22.11";
    };
  };

  outputs = { self, nixpkgs }: {
    nixosConfigurations = {
      x1-2021 = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./system/configuration.nix
        ];
      };
    };
  };
}
