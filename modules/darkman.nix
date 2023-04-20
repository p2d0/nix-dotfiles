{ config, lib, pkgs, ... }:

with lib;
with pkgs.my;
let cfg = config.modules.darkman;
in {
  options.modules.darkman = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    xdg.portal = {
      enable = true;
      extraPortals = [ darkman ];
    };
    services = {
      dbus = {
        enable = true;
        packages = [darkman];
      };
    };
    (my.allUsers ({}: {
      xdg.systemDirs.data = [
        "/etc/nixos/configs/darkman"
      ];
    }));
    environment.systemPackages = [
      darkman
    ];
  };
}
