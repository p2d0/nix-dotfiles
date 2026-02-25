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
          # 1. Set the color scheme preference (Primary for modern apps/Firefox)
          ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
          # 2. Set the legacy theme (For GTK3 apps)
          ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
        '';
        hyprpaper = ''${pkgs.swww}/bin/swww img /etc/nixos/bg_old.png --transition-type center'';
      };
      lightModeScripts = {
        gtk-theme = ''
          ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'
          ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
        '';
        hyprpaper = ''${pkgs.swww}/bin/swww img /etc/nixos/light.jpg --transition-type center'';
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
