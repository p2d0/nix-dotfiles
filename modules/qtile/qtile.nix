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
  config = mkIf cfg.enable (my.allUsers ({}: {
    home.file = {
      ".config/qtile".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/qtile;
    };
  }));
}
