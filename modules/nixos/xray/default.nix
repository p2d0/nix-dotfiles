{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.xray;
in {
  options.modules.xray = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.services.xray = {
      enable = true;
      description = "Xray";
      wantedBy = [ "default.target" ];
      script = ''
              # Function to run Xray
              function run_xray {
                  ${pkgs.xray}/bin/xray run -c /home/andrew/Dropbox/xray/config.json &
              }

              # Function to check if Xray is running
              function check_xray_running {
                  if ${pkgs.procps}/bin/pgrep .xray-wrapped > /dev/null; then
                      return 0  # Xray is running
                  else
                      return 1
                  fi
              }

              # Function to restart Xray
              function restart_xray {
                  if check_xray_running; then
                      echo "Xray is running. Restarting..."
                      ${pkgs.procps}/bin/pkill .xray-wrapped
                      run_xray
                  else
                      echo "Xray is not running. Starting..."
                      run_xray
                  fi
              }

              # Initial Xray run
              restart_xray

              # Set up inotifywait to monitor file changes
              ${pkgs.inotify-tools}/bin/inotifywait -m -e modify /home/andrew/Dropbox/xray/config.json | while read -r line; do
                  echo "Config file modified. Restarting Xray..."
                  restart_xray
              done
        '';
      # serviceConfig = {
      #   Type = "simple";
      #   # ExecStart = "${pkgs.bash}/bin/bash /etc/nixos/modules/nixos/vpn/xray.sh";
      #   # ExecStart = "${pkgs.xray}/bin/xray run -c /home/andrew/Dropbox/xray/config.json";
      # };
    };
  };
}
