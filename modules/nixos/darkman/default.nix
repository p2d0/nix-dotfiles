{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.darkman;
    darkman = pkgs.unstable.darkman.overrideAttrs(oldAttrs: rec {
      postPatch = oldAttrs.postPatch + ''
    sed -i '16a Environment=PATH=/run/current-system/sw/bin' darkman.service'';
    });
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
      extraPortals = [ darkman ];
    };
    services = {
      dbus = {
        enable = true;
        packages = [darkman];
      };
    };

    environment.systemPackages = [
      darkman
    ];
  } // (my.allUsers ({...}: { # TODO could be a better function

    home.file = {
      ".config/darkman".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/darkman;
    };
    systemd.user.services.darkman = {
      Unit = {
        Description = "Darkman system service";
        Documentation = "man:darkman(1)";
        PartOf = [ "graphical-session.target" ];
        BindsTo = [ "graphical-session.target" ];
        # X-Restart-Triggers =
        #   [ "${config.xdg.configFile."darkman/config.yaml".source}" ];
      };

      Service = {
        Type = "dbus";
        BusName = "nl.whynothugo.darkman";
        Environment = "PATH=/run/current-system/sw/bin";
        ExecStart = "${pkgs.darkman}/bin/darkman run";
        Restart = "on-failure";
        TimeoutStopSec = 15;
        Slice = "background.slice";
      };

      Install.WantedBy = [ "graphical-session.target" ];
    };
    xdg.systemDirs.data = [
      "/etc/nixos/configs/darkman"
    ];
  })));
}
