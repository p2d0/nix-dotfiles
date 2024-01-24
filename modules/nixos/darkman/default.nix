{ config, lib, pkgs, ... }:

with lib;
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
  config = mkIf cfg.enable ({
    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.my.darkman ];
    };
    services = {
      dbus = {
        enable = true;
        packages = [pkgs.my.darkman];
      };
    };
    systemd.packages = [
      pkgs.my.darkman
    ];
    environment.systemPackages = [
      pkgs.my.darkman
    ];
  } // (my.allUsers ({...}: { # TODO could be a better function
    xdg.systemDirs.data = [
      "/etc/nixos/configs/darkman"
    ];
  })));
}
