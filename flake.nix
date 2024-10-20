{
  # --fake 8 --ttl 5
  # --disorder 1 --auto=torst --fake -1 --tlsrec 3+h
  # --fake -1 --md5sig
  description = "An example NixOS configuration";
  nixConfig = {
    extra-substituters = [
      "https://nix-community.cachix.org"
    ];
    extra-trusted-public-keys = [
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
  # Searching github for packages:
  # %package_name% language:nix
  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-24.05"; };
    nixpkgs-23 = { url = "github:nixos/nixpkgs/nixos-23.11"; };
    nixos-unstable.url = "nixpkgs/nixos-unstable";
    nixos-master.url = "github:nixos/nixpkgs/master";
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    hyprland.url = "github:hyprwm/Hyprland";
    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    poetry2nix.url = "github:nix-community/poetry2nix";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    compfy.url = "github:allusive-dev/compfy";
    spl3g-config.url = "github:spl3g/nixfiles";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
  };

  outputs = inputs @
    {self,
      nixpkgs,
      nixpkgs-23,
      poetry2nix,
      chaotic,
      zen-browser,
      compfy,
      nixos-unstable,
      nixos-master,
      hyprland,
      home-manager,
      spl3g-config,
      ... }:
        let lib = nixpkgs.lib.extend (self: super: { my = import ./lib/util.nix { lib = nixpkgs.lib; }; });
            nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
            system =  "x86_64-linux";
            mkPkgs = pkgs: extraOverlays: import pkgs {
              # TODO convert to pkg-sets https://github.com/nix-community/home-manager/issues/1538#issuecomment-706627100
              inherit system;
              config.allowBroken = true;
              config.allowUnfree = true;  # forgive me Stallman senpai
              config.permittedInsecurePackages = [
                "xrdp-0.9.9" "python-2.7.18.8-env" "python-2.7.18.8" "python-2.7.18.7-env" "python-2.7.18.7" "libdwarf-20181024" "python-2.7.18.6" ];
              overlays = [(self: super: {
                inherit lib;
                # fish get-pr-override 218037
                pr218037 = import (fetchTarball
                  "${nixpkgs-tars}84963237b438319092a352a7d375878d82beb1ca.tar.gz") {
                    config = self.config;
                  };
                pr314293 = import (fetchTarball
                  "${nixpkgs-tars}dd0629f12ebf19510a682ff132265253b7728ccc.tar.gz")
                  { config = self.config; };
                pr229886 = import (fetchTarball
                  "https://github.com/NixOS/nixpkgs/archive/pull/229886/head.tar.gz") {
                    config = self.config;
                  };
                flacpkgs = import (builtins.fetchTarball {
                  url = "https://github.com/NixOS/nixpkgs/archive/cea111161b02f1a823698038bfd01bc93607e391.tar.gz";
                }) {};
                # gvfspkgs = import (builtins.fetchTarball {
                #   url = "https://github.com/NixOS/nixpkgs/archive/c3f5dc69cdff75a054db50eec743e0693d296978.tar.gz";
                # }) {};

                old-23 = import nixpkgs-23 { config = self.config; };
                unstable = import nixos-unstable { config = self.config; };
                master = import nixos-master { config = self.config; };
                compfy = compfy.packages.${system}.compfy;
                zen-browser = zen-browser.packages.${system}.default;
                my = lib.my.mapModules ./pkgs (p: self.callPackage p {});
                tgs2png = spl3g-config.overlays.additions.tgs2png;
              })] ++ extraOverlays;
            };
            pkgs  = mkPkgs nixpkgs [ poetry2nix.overlays.default chaotic.overlays.default spl3g-config.overlays.additions];
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
              default = final: prev: pkgs.my;
              # compfy = compfy;
            };
            # user-modules = (lib.my.mapModulesRec' ./modules/nixos (p: pkgs.callPackage p {}));

            # nix eval .#tests --impure
            tests = pkgs.callPackage ./tests/default.nix {};

            nixosConfigurations = {
              mysystem = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                modules = (lib.my.findAllModulePathsIn ./modules/nixos) ++ [
                  {nixpkgs.pkgs = pkgs;}
                  {
                    # pin system nixpkgs to the same version as the flake input
                    # (don't see a way to declaratively set channels but this seems to work fine?)
                    nix.nixPath = [
                      "nixpkgs=${nixpkgs}"
                    ];
                  }
                  home-manager.nixosModules.home-manager
                  hyprland.nixosModules.default
                  ./home.nix
                  ./hosts/main/configuration.nix
                ];
                specialArgs = { inherit self inputs lib;};
              };
            };
          };
}
