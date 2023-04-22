{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.vpn;
in {
  options.modules.vpn = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;
      [
        v2ray
        # my.psiphon
        # my.lantern
      ];

  };
}
