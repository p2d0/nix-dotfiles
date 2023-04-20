{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.timed-shutdown;
in {
  options.modules.timed-shutdown = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
    time = mkOption {
      type = types.string;
      default = "21:00:00";
      example = "21:00:00";
      description = ''
      Time in format HH:mm:ss
      '';
    };

  };
  config = mkIf cfg.enable {
    systemd.services.shutdown = {
      description = "Shutdown service";
      serviceConfig.Type = "oneshot";
      script = "shutdown now";
    };

    systemd.timers.shutdown = {
      description = "Shutdown timer";
      wantedBy = [ "timers.target" ];
      timerConfig.OnCalendar = "*-*-* ${cfg.time}";
      timerConfig.Unit = "shutdown.service";
    };

  };
}
