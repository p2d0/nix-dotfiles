{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.kmonad;
in {
  options.modules.kmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = [
      kmonad
    ];
    systemd.services.kmonad =  {
      enable = true;
      description = "kmonad";
      wantedBy = [ "graphical.target" ];
      serviceConfig = {
        ExecStart =
          "kmonad /etc/nixos/modules/nixos/kmonad/default.kbd";
      };
    };
  };
}
