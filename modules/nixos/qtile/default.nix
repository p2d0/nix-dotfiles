{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.qtile;
in {
  options.modules.qtile = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    nixpkgs.config.packageOverrides = pkgs: {
      qtile = (pkgs.callPackage /etc/nixos/pkgs/qtile.nix {});
    };
  } // (my.allUsers ({}: {
    home.file = {
      ".config/qtile".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/qtile;
    };
  }));
}
