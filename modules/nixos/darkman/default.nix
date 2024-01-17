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
    # systemd.user.services.darkman = {
    #   Unit = {
    #     After = [ "default.target" ];
    #     PartOf = [ "default.target" ];
    #   };
    # };

    systemd.user.services.dark = {
      enable = true;
      description = "Darkman";
      wantedBy = [ "default.target" ];
      serviceConfig = {
        ExecStart =
          "${pkgs.my.darkman}/bin/darkman run";
        Environment = ["PATH=/run/current-system/sw/bin"];
      };
    };

    # systemd.packages = [
    #   pkgs.my.darkman
    # ];
    environment.systemPackages = [
      pkgs.my.darkman
    ];
  } // (my.allUsers ({...}: { # TODO could be a better function
    xdg.systemDirs.data = [
      "/etc/nixos/configs/darkman"
    ];
  })));
}
