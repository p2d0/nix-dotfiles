{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.eww;
in {
  options.modules.eww = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {

    home.file = {
      ".config/eww" = {
        source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/eww;
      };
    };

    home.packages = with pkgs;[
      eww
    ];
  };
}
