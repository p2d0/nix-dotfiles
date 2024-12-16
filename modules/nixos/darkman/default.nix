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
        gtk-theme = ''
${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-dark'"
${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/gtk-theme "'Adwaita-dark'"
${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/icon-theme "'Adwaita-dark'"
'';
        hyprpaper = ''
${pkgs.swww}/bin/swww img  /etc/nixos/bg_old.png --transition-type center --transition-fps 75'';

      };
      lightModeScripts = {
        gtk-theme = ''
${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/color-scheme "'prefer-light'"
${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/gtk-theme "'Adwaita'"
${pkgs.dconf}/bin/dconf write /org/gnome/desktop/interface/icon-theme "'Adwaita'"
'';
        # exec-once = systemctl --user import-environment HYPRLAND_INSTANCE_SIGNATURE
        # in hyprland config if no signature
        hyprpaper = ''
${pkgs.swww}/bin/swww img /etc/nixos/light.jpg --transition-type center --transition-fps 75'';
      };
      settings = {
        lat = 55.7;
        lng = 37.6;
        usegeoclue = false;
      };
    };

    systemd.user.services.darkman = {
      Unit = {
        After = ["swww.service"];
      };
    };
  })));
}
