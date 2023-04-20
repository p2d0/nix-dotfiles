{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.emacs-with-doom;
in {
  options.modules.emacs-with-doom = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (import (builtins.fetchTarball {
        url =
          "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
        # sha256 = "sha256:195k5y6p2apy6bz3xm7vklsfm3h4vx2m412sinrzrjzxb3b5rgcj";
      }))
    ];
    services.emacs.install = true;
    services.emacs.enable = false;
    services.emacs.defaultEditor = true;
    # services.emacs.package = pkgs.emacsUnstable.override {
    #   withGTK3 = true;
    # };
    services.emacs.package = pkgs.emacsUnstable.overrideAttrs(oldAttrs: rec {
      src = pkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "emacs";
        rev = "json-rpc";
        sha256 = "sha256-mnSG1MqUapaXyHHJRHv40cWUx1zRIwTM1O810ZJgRgc=";
      };
    });

    (my.allUsers ({}: {
      home.file = {
        "${config.home.homeDirectory}/.doom.d".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/editors/.doom.d;
      };
    }));
  };

}
