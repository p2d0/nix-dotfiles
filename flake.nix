{
  description = "An example NixOS configuration";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-22.05"; };
    home-manager.url = "github:nix-community/home-manager/release-22.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, ... }:
    {
      nixosConfigurations = {
        mysystem = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            home-manager.nixosModules.home-manager
            ./modules/hjkl/hjkl.nix
            ./modules/fonts/fonts.nix
            ./andrew.nix
            ./andrew-work.nix
            ./modules/options.nix
            ./modules/pipewire.nix
            ./hosts/main/configuration.nix
          ];
          specialArgs = { inherit inputs; };
        };
      };
    };
}
