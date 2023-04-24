{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.printing3d;
in {
  options.modules.printing3d = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;[
      blender
      cura
      freecad
    ];
  };
}
