{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.warp;
in {
  options.modules.warp = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.services.warp-svc = {
      enable = true;
      description = "Warp service";
      wantedBy = [ "default.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.my.warp}/bin/warp-svc";
      };
    };
    environment.systemPackages = [
      pkgs.my.warp
    ];
  };
}
