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
        ExecStop = "/bin/kill -SIGTERM $MAINPID";
      };
    };

    systemd.user.timers.trex-start = {
      enable = true;
      wantedBy = [ "timers.target"];
      timerConfig = {
        OnCalendar = "*-*-* 22:00:00";
        Unit = "trex.service";
      };
    };

    # systemd.user.timers.trex-stop = {
    #   wantedBy = [ "timers.target"];

    #   timerConfig = {
    #     OnCalendar = "*-*-* 08:30:00";
    #     Unit = "trex.service";
    #   };
    # };
  };
}
