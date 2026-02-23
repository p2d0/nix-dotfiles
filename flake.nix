{
  # --fake 8 --ttl 5
  # --disorder 1 --auto=torst --fake -1 --tlsrec 3+h
  # --fake -1 --md5sig
  description = "An example NixOS configuration";
  nixConfig = {
    substitute = true;
    extra-substituters = [
      "https://nix-community.cachix.org"
      "https://hyprland.cachix.org"
      "https://upgradegamma.cachix.org"
    ];
    extra-trusted-public-keys = [
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "upgradegamma.cachix.org-1:iIifduPUNZ9OrRYgaEcKTeRQxbqr2/FbiF1bboND05A="
    ];
  };
  # Searching github for packages:
  # %package_name% language:nix
  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixos-25.11"; };
    nixpkgs-23 = { url = "github:nixos/nixpkgs/nixos-23.11"; };
    nixpkgs-24-11 = { url = "github:nixos/nixpkgs/nixos-24.11"; };
    nixpkgs-hyprland.url = "github:nixos/nixpkgs/762a398892576efcc76fb233befbd58c2cef59e0";
    nixpkgs-hy3.url = "github:nixos/nixpkgs/d98abf5cf5914e5e4e9d57205e3af55ca90ffc1d";
    nixos-unstable.url = "nixpkgs/nixos-unstable";
    # nanobanana.url = "path:/mnt/new/ai/projects/nanobanana"; 
    nixos-master.url = "github:nixos/nixpkgs/master";
    void-editor.url = "github:bariscodefxy/void-editor-flake";
    home-manager.url = "github:nix-community/home-manager/release-25.11";
    # hyprland.url = "github:hyprwm/Hyprland/";
    # hyprland = {
    #   url = "git+https://github.com/hyprwm/Hyprland.git?ref=v0.47.0";
    # };
    tdesktop = {
      url = "git+https://github.com/p2d0/tdesktop.git?ref=refs/heads/p2d0_update&allRefs=1&submodules=1";
    };

    quickshell = {
      url = "git+https://git.outfoxxed.me/outfoxxed/quickshell";

      # THIS IS IMPORTANT
      # Mismatched system dependencies will lead to crashes and other issues.
      inputs.nixpkgs.follows = "nixpkgs";
    };

    chaotic.url = "github:chaotic-cx/nyx/nyxpkgs-unstable";
    poetry2nix.url = "github:nix-community/poetry2nix";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    compfy.url = "github:allusive-dev/compfy";
    spl3g-config.url = "github:spl3g/nixfiles";
    zen-browser.url = "github:0xc000022070/zen-browser-flake";
    nixpkgs-amnezia.url = "github:averyanalex/nixpkgs/amneziawg";
    # hy3 = {
    #   url = "github:outfoxxed/hy3?ref=hl0.47.0-1";
    #   inputs.hyprland.follows = "hyprland";
    # };
  };

  outputs = inputs @
    {self,
      nixpkgs,
      nixpkgs-23,
      nixpkgs-24-11,
      poetry2nix,
      chaotic,
      tdesktop,
      zen-browser,
      compfy,
      # hy3,
      nixpkgs-hyprland,
      nixpkgs-hy3,
      nixos-unstable,
      nixos-master,
      void-editor,
      # hyprland,
      nixpkgs-amnezia,
      # nanobanana,
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
                "qtwebengine-5.15.19"

                "python3.11-youtube-dl-2021.12.17"
                "mbedtls-2.28.10"
                "ventoy-gtk3-1.1.05"
                "ventoy-gtk3-1.1.07"
                "qbittorrent-qt5-4.6.4"
                "libsoup-2.74.3"
                "dotnet-runtime-6.0.36"
                "dotnet-sdk-wrapped-6.0.428"
                "dotnet-sdk-6.0.428"
                "dotnet-runtime-wrapped-6.0.36"
                "qbittorrent-4.6.4" "xrdp-0.9.9" "python-2.7.18.8-env" "python-2.7.18.8" "python-2.7.18.7-env" "python-2.7.18.7" "libdwarf-20181024" "python-2.7.18.6" ];
              overlays = [(self: super: {
                inherit lib;
                # fish get-pr-override 218037
                pr218037 = import (fetchTarball
                  "${nixpkgs-tars}84963237b438319092a352a7d375878d82beb1ca.tar.gz") {
                    config = self.config;
                  };
                amnezia = import nixpkgs-amnezia {
                  inherit system;
                  config = self.config;
                };
                pr314293 = import (fetchTarball
                  "${nixpkgs-tars}dd0629f12ebf19510a682ff132265253b7728ccc.tar.gz")
                  { config = self.config; };
                pr229886 = import (fetchTarball
                  "https://github.com/NixOS/nixpkgs/archive/pull/229886/head.tar.gz") {
                    config = self.config;
                  };
                pr419945 = import (fetchTarball
                  "${nixpkgs-tars}6574cde9e881474ca4ee3438d4ad0ab471f58b0b.tar.gz")
                  { config = self.config; };

                flacpkgs = import (builtins.fetchTarball {
                  url = "https://github.com/NixOS/nixpkgs/archive/cea111161b02f1a823698038bfd01bc93607e391.tar.gz";
                }) {};
                # gvfspkgs = import (builtins.fetchTarball {
                #   url = "https://github.com/NixOS/nixpkgs/archive/c3f5dc69cdff75a054db50eec743e0693d296978.tar.gz";
                # }) {};

                sounduxPkgs = import (builtins.fetchTarball {
                  url = "https://github.com/NixOS/nixpkgs/archive/78747312c7c8978a140c0a3ad236766289aecf8b.tar.gz";
                }) {};
                openssl_3_2Pkgs = import (builtins.fetchTarball {
                  url = "https://github.com/NixOS/nixpkgs/archive/007604529b133db7ef40e687cd95d88c37e5fe8a.tar.gz";
                }) {};
                quickshell = inputs.quickshell.packages.${system}.default;
                void-editor = inputs.void-editor.packages.${system}.default;

                old-23 = import nixpkgs-23 { config = self.config; };
                old-24-11 = import nixpkgs-24-11 { config = self.config; };
                unstable = import nixos-unstable { config = self.config; };
                nixpkgs-hyprland = import nixpkgs-hyprland { config = self.config; };
                nixpkgs-hy3 = import nixpkgs-hy3 { config = self.config; };
                master = import nixos-master { config = self.config; };
                compfy = compfy.packages.${system}.compfy;
                zen-browser = zen-browser.packages.${system}.default;
                tdesktop_p2d0 = tdesktop.packages.${system}.default;
                # my = lib.my.mapModules ./pkgs (p:
                #   let eval = builtins.tryEval (self.callPackage p {});
                #   in
                #     if eval.success then eval.value else builtins.throw "Package error " ++ p );
                my = lib.my.mapModules ./pkgs (p:  (self.callPackage (lib.debug.traceVal p) {}));
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
            # kek = pkgs.my.apollo;
            tests = pkgs.callPackage ./tests/default.nix {};
            nix.nixPath = [
              "nixpkgs=${pkgs.path}"
              "unstable=${nixos-unstable}"
            ];

            nixosConfigurations = {
              mysystem = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                modules = (lib.my.findAllModulePathsIn ./modules/nixos) ++ [
                  {nixpkgs.pkgs = pkgs;}
                  {
                    # pin system nixpkgs to the same version as the flake input
                    # (don't see a way to declaratively set channels but this seems to work fine?)
                    nix.nixPath = [
                      "nixpkgs=${pkgs.path}"
                      "unstable=${nixos-unstable}"
                    ];
                  }
                  home-manager.nixosModules.home-manager
                  # nanobanana.nixosModules.default
                  # hyprland.nixosModules.default
                  ./home.nix
                  ./hosts/main/configuration.nix
                  ./hosts/common.nix
                ];
                specialArgs = { inherit self inputs lib;};
              };

              laptop = nixpkgs.lib.nixosSystem {
                system = "x86_64-linux";
                modules = (lib.my.findAllModulePathsIn ./modules/nixos) ++ [
                  {nixpkgs.pkgs = pkgs;}
                  {
                    # pin system nixpkgs to the same version as the flake input
                    # (don't see a way to declaratively set channels but this seems to work fine?)
                    nix.nixPath = [
                      "nixpkgs=${pkgs.path}"
                      "unstable=${nixos-unstable}"
                    ];
                  }
                  home-manager.nixosModules.home-manager
                  # nanobanana.nixosModules.default
                  # hyprland.nixosModules.default
                  ./home-laptop.nix
                  ./hosts/laptop/configuration.nix
                  ./hosts/common.nix
                ];
                specialArgs = { inherit self inputs lib;};
              };
            };
          };
}
