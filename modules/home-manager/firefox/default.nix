{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.firefox;
in {
  options.modules.firefox = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable
    {
    };
}
