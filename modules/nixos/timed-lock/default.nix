{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.timed-lock;
in {
  options.modules.timed-lock = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the auto-lock timer.";
    };
    times = mkOption {
      # We change this to a list of strings
      type = types.listOf types.str;
      default = [ "21:00:00" "21:05:00" "21:15:00" "21:30:00" ];
      example = [ "12:00:00" "21:00:00" ];
      description = ''
        List of times in format HH:mm:ss.
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.auto-lock = {
      description = "Auto-lock";
      serviceConfig = {
        ExecStart = "${pkgs.hyprlock}/bin/hyprlock";
        Type = "simple";
      };
    };

    systemd.user.timers.auto-lock = {
      description = "Timer for auto-lock";
      timerConfig = {
        # We use 'map' to turn each time into a full OnCalendar string
        OnCalendar = map (time: "*-*-* ${time}") cfg.times;
        Persistent = true;
      };
      wantedBy = [ "timers.target" ];
    };
  };
}
