{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.singbox;
in {
  options.modules.singbox = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.services.singbox = {
      enable = true;
      description = "Sing-box hiddify";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "simple";
        # ExecStart = "/etc/nixos/modules/nixos/vpn/xray.sh";
        ExecStart = "${pkgs.my.singbox}/bin/sing-box run -c /home/andrew/Dropbox/singbox/config.json";
        Restart = "on-failure";
        RestartSec = 3;
      };
    };
  };
}
