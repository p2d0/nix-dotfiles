{
  description = "An example NixOS configuration";

  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-22.11"; };
    home-manager.url = "github:nix-community/home-manager/release-22.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs @ { self, nixpkgs, home-manager, ... }:
    let lib = nixpkgs.lib.extend (self: super: { my = import /etc/nixos/lib/multiuser.nix { }; });
        system =  "x86_64-linux";
        mkPkgs = pkgs: extraOverlays: import pkgs {
          inherit system;
          config.allowBroken = true;
          config.allowUnfree = true;  # forgive me Stallman senpai
          overlays = [(self: super: {inherit lib;})];
        };
        pkgs  = mkPkgs nixpkgs [ self.overlay ];
    in
      {
        inherit lib;
        nixosConfigurations = {
          mysystem = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              {nixpkgs.pkgs = pkgs;}
              home-manager.nixosModules.home-manager
              ./modules/hjkl/hjkl.nix
              ./modules/fonts/fonts.nix
              ./modules/fcitx.nix
              ./andrew.nix
              ./andrew-work.nix
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

          mysystem-light = inputs.nixpkgs.lib.nixosSystem {
            inherit system;
            modules = [
              {nixpkgs.pkgs = pkgs;}
              home-manager.nixosModules.home-manager
              ./modules/hjkl/hjkl.nix
              ./modules/fonts/fonts.nix
              ./modules/fcitx.nix
              ./andrew-light.nix
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