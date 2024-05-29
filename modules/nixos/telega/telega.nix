{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.telega;
in {
  options.modules.telega = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {

  };
}
