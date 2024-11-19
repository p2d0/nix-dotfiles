{ config, lib, pkgs, ... }:
# Older versions
# https://lazamar.co.uk/nix-versions/
with lib;
let cfg = config.modules.vpn;
    oldPkgs = import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/c2c0373ae7abf25b7d69b2df05d3ef8014459ea3.tar.gz";
    }) {};
in {
  options.modules.vpn = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };

  config = mkIf cfg.enable (lib.my.withHome
    (args: {
      home.file = {
        ".config/qv2ray".source = args.config.lib.file.mkOutOfStoreSymlink /home/${config.user}/Dropbox/qv2ray;
      };})
    {
      environment.systemPackages = with pkgs;
        [
          pkgs.v2ray
          pkgs.qv2ray
          pkgs.unstable.nekoray
          pkgs.xray
          pkgs.my.singbox
          # (pkgs.sing-box.overrideAttrs(oldAttrs: rec {
          #   src = fetchFromGitHub {
          #     owner = "hiddify";
          #     repo = "hiddify-sing-box";
          #     rev = "master";
          #     sha256 = "sha256-TZuEcLWHHmjlH0g0baPOurqFIHzppkYCHL1Dif8wLsk=";
          #   };
          #   vendorHash = "kekes";
          # }))

          # my.psiphon
          # my.lantern
        ];

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
      };});
}
