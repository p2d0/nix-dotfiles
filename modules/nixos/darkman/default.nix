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

  } // (my.allUsers ({...}: {
    services.darkman = {
      enable = true;
      darkModeScripts = {
        gtk-theme = ''${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"'';
        hyprpaper = ''
${pkgs.hyprland}/bin/hyprctl hyprpaper wallpaper ",/etc/nixos/bg_old.png" '';

      };
      lightModeScripts = {
        gtk-theme = ''${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'" '';
        # exec-once = systemctl --user import-environment HYPRLAND_INSTANCE_SIGNATURE
        # in hyprland config if no signature
        hyprpaper = ''
${pkgs.hyprland}/bin/hyprctl hyprpaper wallpaper ",/etc/nixos/light.jpg" '';
      };
      settings = {
        lat = 55.7;
        lng = 37.6;
        usegeoclue = false;
      };
    };
  })));
}
