{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.maestral;
in {
  options.modules.maestral = {
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
      pkgs.maestral
      # pkgs.maestral-gui
    ];
    systemd.user.services.maestral = {
      description = "Maestral daemon";
      wantedBy = [ "graphical-session.target" ];
      After = ["swww.service"];
      serviceConfig = {
        ExecStart = "${pkgs.maestral}/bin/maestral start -f";
        ExecStop = "${pkgs.maestral}/bin/maestral stop";
        Restart = "on-failure";
        Nice = 10;
      };
    };
  };
}
