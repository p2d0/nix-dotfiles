# NOTE:
# cfg.configFile contains secrets such as proxy servers' credential!
# we dont want plaintext secrets in world-readable `/nix/store`.

{
  lib,
  config,
  pkgs,
  ...
}:
let
  cfg = config.modules.mihomo;
in
{
  options.modules.mihomo = {
    enable = lib.mkEnableOption "Mihomo, A rule-based proxy in Go";

    package = lib.mkPackageOption pkgs "mihomo" { };

    configFile = lib.mkOption {
      type = lib.types.path;
      description = "Configuration file to use.";
    };

    webui = lib.mkOption {
      default = null;
      type = lib.types.nullOr lib.types.path;
      example = lib.literalExpression "pkgs.metacubexd";
      description = ''
        Local web interface to use.

        You can also use the following website:
        - metacubexd:
          - https://d.metacubex.one
          - https://metacubex.github.io/metacubexd
          - https://metacubexd.pages.dev
        - yacd:
          - https://yacd.haishan.me
        - clash-dashboard:
          - https://clash.razord.top
      '';
    };

    extraOpts = lib.mkOption {
      default = null;
      type = lib.types.nullOr lib.types.str;
      description = "Extra command line options to use.";
    };

    tunMode = lib.mkEnableOption ''
      necessary permission for Mihomo's systemd service for TUN mode to function properly.

      Keep in mind, that you still need to enable TUN mode manually in Mihomo's configuration
    '';
  };

  config = lib.mkIf cfg.enable {
    ### systemd service
    systemd.services."mihomo" = {
      description = "Mihomo daemon, A rule-based proxy in Go.";
      documentation = [ "https://wiki.metacubex.one/" ];
      requires = [ "network-online.target" ];
      after = [ "network-online.target" ];
      wantedBy = [ "multi-user.target" ];
      environment = {
        SKIP_SAFE_PATH_CHECK = "1";
      };
      script = ''
              # Function to run Mihomo
              function run_mihomo {
                  ${cfg.package}/bin/mihomo -d /var/lib/private/mihomo -f ${cfg.configFile} &
              }

              # Function to check if Mihomo is running
              function check_mihomo_running {
                  if ${pkgs.procps}/bin/pgrep -f ${cfg.package} > /dev/null; then
                      return 0  # Mihomo is running
                  else
                      return 1
                  fi
              }

              # Function to restart Mihomo
              function restart_mihomo {
                  if check_mihomo_running; then
                      echo "Mihomo is running. Restarting..."
                      ${pkgs.procps}/bin/pkill -f ${cfg.package}
                      run_mihomo
                  else
                      echo "Mihomo is not running. Starting..."
                      run_mihomo
                  fi
              }

              # Initial Mihomo run
              restart_mihomo
              sleep 3
              restart_mihomo

              # Set up inotifywait to monitor file changes
              ${pkgs.inotify-tools}/bin/inotifywait -m -e modify /home/${config.user}/Dropbox/mihomo/config.yaml | while read -r line; do
                  echo "Config file modified. Restarting Mihomo..."
                  restart_mihomo
              done
'';
      serviceConfig =
        {
          # ExecStart = lib.concatStringsSep " " [
          #   (lib.getExe cfg.package)
          #   "-d /var/lib/private/mihomo"
          #   "-f \${CREDENTIALS_DIRECTORY}/config.yaml"
          #   (lib.optionalString (cfg.webui != null) "-ext-ui ${cfg.webui}")
          #   (lib.optionalString (cfg.extraOpts != null) cfg.extraOpts)
          # ];

          DynamicUser = true;
          StateDirectory = "mihomo";
          # LoadCredential = "config.yaml:${cfg.configFile}";

          ### Hardening
          # AmbientCapabilities = "";
          # CapabilityBoundingSet = "";
          # DeviceAllow = "";
          # LockPersonality = true;
          # MemoryDenyWriteExecute = true;
          # NoNewPrivileges = true;
          # PrivateDevices = true;
          # PrivateMounts = true;
          # PrivateTmp = true;
          # PrivateUsers = true;
          # ProcSubset = "pid";
          # ProtectClock = true;
          # ProtectControlGroups = true;
          # ProtectHome = true;
          # ProtectHostname = true;
          # ProtectKernelLogs = true;
          # ProtectKernelModules = true;
          # ProtectKernelTunables = true;
          # ProtectProc = "invisible";
          # ProtectSystem = "strict";
          # RestrictRealtime = true;
          # RestrictSUIDSGID = true;
          # RestrictNamespaces = true;
          # RestrictAddressFamilies = "AF_INET AF_INET6";
          # SystemCallArchitectures = "native";
          # SystemCallFilter = "@system-service bpf";
          # UMask = "0077";
        }
        // lib.optionalAttrs cfg.tunMode {
          AmbientCapabilities = "CAP_NET_ADMIN CAP_NET_RAW CAP_NET_BIND_SERVICE CAP_SYS_TIME CAP_SYS_PTRACE CAP_DAC_READ_SEARCH";
          CapabilityBoundingSet = "CAP_NET_ADMIN CAP_NET_RAW CAP_NET_BIND_SERVICE CAP_SYS_TIME CAP_SYS_PTRACE CAP_DAC_READ_SEARCH";
          # PrivateDevices = false;
          # PrivateUsers = false;
          # RestrictAddressFamilies = "AF_INET AF_INET6 AF_NETLINK";
        };
    };
  };

  meta.maintainers = with lib.maintainers; [ Guanran928 ];
}
