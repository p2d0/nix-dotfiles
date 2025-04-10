{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.byedpi;
in {
  options.modules.byedpi = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.byedpi = {
      enable = true;
      description = "byedpi";
      wantedBy = [ "default.target" ];
      serviceConfig = {
        ExecStart =
          # "${pkgs.my.byedpi}/bin/ciadpi --disorder 1 --auto=torst --fake -1 --tlsrec 3+h";
          # "${pkgs.my.byedpi}/bin/ciadpi --fake -1 --md5sig --auto=torst --disorder 1 --auto=torst --fake -1 --tlsrec 3+h";
          "${pkgs.my.byedpi}/bin/ciadpi --fake -1 --md5sig";
      };
    };
  };
}
