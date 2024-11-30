{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.torrent;
in {
  options.modules.torrent = {
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
      pkgs.qbittorrent-nox
    ];
    systemd.services.qbittorrent = {
      enable = true;
      description = "Qbittorrent nox";
      wantedBy = [ "graphical-session" ];
      serviceConfig = {
        ExecStart =
          "${pkgs.qbittorrent-nox}";
      };
    };
  };
}
