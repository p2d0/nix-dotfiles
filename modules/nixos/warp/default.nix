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
    # systemd.services.warp-svc = {
    #   enable = true;
    #   description = "Warp service";
    #   wantedBy = [ "default.target" ];
    #   serviceConfig = {
    #     ExecStart = "${pkgs.cloudflare-warp}/bin/warp-svc";
    #   };
    # };
    systemd.packages = [
      pkgs.cloudflare-warp
    ];
    environment.systemPackages = [
      pkgs.cloudflare-warp
    ];
  };
}
