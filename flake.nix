{ config, lib, pkgs, ... }:

{
  description = "An example NixOS configuration";

  inputs = {
  };

  outputs = inputs @ { self, nixpkgs, ... }:
    {
      nixosConfigurations = {
        mysystem = inputs.nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            ./configuration.nix
          ];
          specialArgs = { inherit inputs; };
        };
      };
    };
}
