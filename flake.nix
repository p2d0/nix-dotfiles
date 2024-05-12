{
  description = "An example NixOS configuration";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-23.11"; };
    nixos-unstable.url = "nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager/release-23.11";
    hyprland.url = "github:hyprwm/Hyprland";
    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    poetry2nix.url = "github:nix-community/poetry2nix";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    compfy.url = "github:allusive-dev/compfy";
  };

  outputs = inputs @ { self,  nixpkgs, poetry2nix, chaotic, compfy, nixos-unstable, hyprland, home-manager, ... }:
    let lib = nixpkgs.lib.extend (self: super: { my = import /etc/nixos/lib/util.nix { lib = nixpkgs.lib; }; });
        nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
        system =  "x86_64-linux";
        mkPkgs = pkgs: extraOverlays: import pkgs {
          # TODO convert to pkg-sets https://github.com/nix-community/home-manager/issues/1538#issuecomment-706627100
          inherit system;
          config.allowBroken = true;
          config.allowUnfree = true;  # forgive me Stallman senpai
          config.permittedInsecurePackages = [ "xrdp-0.9.9" "libdwarf-20181024" "python-2.7.18.6" ];
          overlays = [(self: super: {
            inherit lib;
            # get-pr-override 218037
            pr218037 = import (fetchTarball
              "${nixpkgs-tars}84963237b438319092a352a7d375878d82beb1ca.tar.gz") {
                config = self.config;
              };
            pr229886 = import (fetchTarball
              "https://github.com/NixOS/nixpkgs/archive/pull/229886/head.tar.gz") {
                config = self.config;
              };
            unstable = import nixos-unstable { config = self.config; };
            compfy = compfy.packages.${system}.compfy;
            my = lib.my.mapModules /etc/nixos/pkgs (p: self.callPackage p {});
          })] ++ extraOverlays;
        };
        pkgs  = mkPkgs nixpkgs [ poetry2nix.overlays.default chaotic.overlays.default ];
    in
      {
        inherit lib;

        user = "andrew";
        # TODO
        # buildSpecific = path: {
        #   pkgs.
        # }
        # overlay =
        #   final: prev: {
        #     my = lib.mapModules ./pkgs (p: pkgs.callPackage p {});
        #   };
        # homeManagerModules = lib.my.mapModulesRec /etc/nixos/modules/home-manager import;
        # nixosModules = lib.

        overlays = {
          default = pkgs.my;
          compfy = compfy;
        };

        nixosConfigurations = {
          mysystem = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              {nixpkgs.pkgs = pkgs;}
              home-manager.nixosModules.home-manager
              hyprland.nixosModules.default
              ./home.nix
              ./hosts/main/configuration.nix
            ] ++ (lib.my.findAllModulePathsIn /etc/nixos/modules/nixos);
            specialArgs = { inherit self inputs lib;};
          };
        };
      };
}
