{
  description = "An example NixOS configuration";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-22.11"; };
    home-manager.url = "github:nix-community/home-manager/release-22.11";
    nixgl.url = "github:guibou/nixGL";
    hyprland.url = "github:hyprwm/Hyprland";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixgl, nixpkgs, hyprland, home-manager, ... }:
    let lib = nixpkgs.lib.extend (self: super: { my = import /etc/nixos/lib/util.nix { lib = nixpkgs.lib; }; });
        nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
        system =  "x86_64-linux";
        mkPkgs = pkgs: extraOverlays: import pkgs {
          inherit system;
          config.allowBroken = true;
          config.allowUnfree = true;  # forgive me Stallman senpai
          config.permittedInsecurePackages = [ "xrdp-0.9.9" "libdwarf-20181024"];
          overlays = [(self: super: {
            inherit lib;
            # get-pr-override 218037
            pr218037 = import (fetchTarball
              "${nixpkgs-tars}84963237b438319092a352a7d375878d82beb1ca.tar.gz") {
                config = self.config;
              };
            my = lib.my.mapModules /etc/nixos/pkgs (p: self.callPackage p {});
          })
                      nixgl.overlay];
        };
        pkgs  = mkPkgs nixpkgs [ self.overlay ];
    in
      {
        inherit lib;

        # overlay =
        #   final: prev: {
        #     my = lib.mapModules ./pkgs (p: pkgs.callPackage p {});
        #   };

        nixosConfigurations = {
          mysystem = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              {nixpkgs.pkgs = pkgs;}
              home-manager.nixosModules.home-manager
              hyprland.nixosModules.default
              ./modules/sway
              ./modules/hjkl/hjkl.nix
              ./modules/fonts/fonts.nix
              ./modules/fcitx.nix
              ./modules/printing3d
              ./modules/keyrings.nix
              ./modules/warp.nix
              ./modules/darkman.nix
              ./modules/timed-shutdown
              ./modules/editors/emacs.nix
              ./andrew.nix
              ./modules/options.nix
              ./modules/pipewire.nix
              ./modules/xmonad/xmonad.nix
              ./modules/jira/jira.nix
              ./modules/qtile/qtile.nix
              ./modules/taffybar/taffybar-home.nix
              ./hosts/main/configuration.nix
            ];
            specialArgs = { inherit inputs lib; };
          };
        };
      };
}
