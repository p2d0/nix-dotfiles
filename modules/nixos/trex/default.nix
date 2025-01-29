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
    systemd.user.services.trex = {
      enable = true;
      description = "trex";
      serviceConfig = {
        ExecStart =
          "${pkgs.my.trex}/bin/t-rex -c /home/${config.user}/Dropbox/trex/config.json";
        # ExecStop = "/bin/kill -SIGTERM $MAINPID";
      };
    };

    systemd.user.timers.trex = {
      enable = true;
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*-*-* 22:10:00";
        Unit = "trex.service";
        Persistent = true;
      };
    };

    systemd.user.services.whoami = {
      enable = true;
      description = "whoami";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.whoami}/bin/whoami";
      };
    };

    systemd.user.services.trex_stop = {
      enable = true;
      description = "stop trex";
      serviceConfig = {
        Type = "oneshot";
        ExecStart = "${pkgs.procps}/bin/pkill -f 't-rex -c /home/${config.user}/Dropbox/trex/config.json'";
      };
    };

    systemd.user.timers.trex_stop = {
      enable = true;
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnCalendar = "*-*-* 07:00:00";
        Unit = "trex_stop.service";
        Persistent = true;
      };
    };
  };
}
