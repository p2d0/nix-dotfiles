{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.hypr;
in {
  options.modules.hypr = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable (lib.my.withHome
    (args:{
      wayland.windowManager.hyprland  = {
        enable = true;
        package = pkgs.unstable.hyprland;
        plugins = [
          pkgs.unstable.hyprlandPlugins.hy3
        ];
        systemd.enable = true;
        xwayland.enable = true;
        systemd.variables = [
          "--all"
        ];
        settings = {
          source = "/etc/nixos/configs/hypr/hyprland.conf";
        };
      };

      systemd.user.services.swww = {
        Install = { WantedBy = [ "graphical-session.target" ]; };

        Unit = {
          ConditionEnvironment = "WAYLAND_DISPLAY";
          Description = "hyprpaper";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };

        Service = {
          ExecStart = "${pkgs.swww}/bin/swww-daemon";
          Restart = "always";
          RestartSec = "10";
        };
      };

      # services.hyprpaper = {
      #   enable = true;
      #   package = pkgs.unstable.hyprpaper;
      #   settings = {
      #     ipc = "on";
      #     preload = [ "/etc/nixos/light.jpg" "/etc/nixos/bg_old.png"];
      #   };
      # };
      #       home.file = {
      # ".config/hypr/hyprpaper.conf".source = args.config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/hypr/hyprpaper.conf;
      # };
    })
    {
      programs.hyprland = {
        enable = true;
        xwayland.enable = true;
      };
      environment.variables = {
        QT_QPA_PLATFORM = "wayland";
        ANKI_WAYLAND = 1;
      };
      xdg.portal = {
        enable = true;
        config.common.default = "*";
        extraPortals = [
          # pkgs.xdg-desktop-portal-hyprland
        ];
      };
      environment.systemPackages = with pkgs;
        [
          gammastep
          swww
          # hyprwall
          grim # screenshot functionality
          slurp # screenshot functionality
          wl-clipboard # wl-copy and wl-paste for copy/paste from stdin / stdout
          wf-recorder
          (unstable.waybar.overrideAttrs(oldAttrs: rec {
            src = fetchFromGitHub {
              owner = "VAWVAW";
              repo = "Waybar";
              rev = "hyprland-bar-scroll";
              sha256 = "sha256-CAV776d4osbQWZp5zHW7zhDdOZkWWGOpXC6+VFarOAs=";
            };
          }))
        ];
    }) ;
}
