{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.trex;
in {
  options.modules.trex = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.services.trex = {
      enable = true;
      description = "trex";
      serviceConfig = {
        ExecStart =
          "${pkgs.my.trex}/bin/t-rex -c /home/${config.user}/Dropbox/trex/config.json";
      };
    };
  };
}
