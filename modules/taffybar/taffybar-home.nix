{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.taffybar;
in {
  options.modules.taffybar = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.gtk-sni-tray = {
      enable = true;
      description = "Gtk sni tray";
      wantedBy = [ "default.target" ];
      serviceConfig = {
        ExecStart =
          "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
      };
    };
  } // (my.allUsers ({}: {
    home.file = {
      ".config/taffybar/taffybar.css" = {
        source = ./taffybar.css;
      };

      ".config/taffybar/gotham.css" = {
        source = ./gotham.css;
      };
    };
  }));
}
