{ config, lib,inputs, pkgs, ... }:

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
    settingsFile = mkOption {
      type = types.str;
      default = "/etc/nixos/configs/hypr/hyprland.conf";
    };
  };
  config = mkIf cfg.enable (lib.my.withHome
    (args:{
      wayland.windowManager.hyprland  = {
        enable = true;
        # package = pkgs.hyprland;
        # package = inputs.hyprland.packages.x86_64-linux.hyprland;
        # package
        package = pkgs.unstable.hyprland;
        # portalPackage = pkgs.unstable.xdg-desktop-portal-hyprland;
        plugins = [
          pkgs.unstable.hyprlandPlugins.hy3
          pkgs.hyprlandPlugins.hyprgrass
          (pkgs.unstable.hyprlandPlugins.hypr-darkwindow.overrideAttrs(oldAttrs: rec {
            patches = [
              /etc/nixos/modules/nixos/hypr/noblur.patch
              /etc/nixos/modules/nixos/hypr/tearing.patch
            ];
          }))
 
          # inputs.hypr-darkwindow.packages.x86_64-linux.Hypr-DarkWindow
          # inputs.hy3.packages.x86_64-linux.hy3
        ];
        systemd.enable = true;
        xwayland.enable = true;
        # withUWSM = true;
        systemd.variables = [
          "--all"
        ];
        settings = {
          source = cfg.settingsFile;
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

      home.file = {
        ".config/hypr/hypridle.conf".source = args.config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/hypr/hypridle.conf;
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
      programs.hyprlock.enable = true;
      environment.variables = {
        qt_qpa_PLATFORM = "wayland";
        ANKI_WAYLAND = 1;
      };
      services.hypridle.enable = true;
      
      # xdg.portal = {
      #   enable = true;
      #   config.common.default = "*";
      #   extraPortals = [
      #     pkgs.xdg-desktop-portal-hyprland
      #   ];
      # };
      programs.waybar = {
        enable = true;
       package = pkgs.my.waybar;
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
          # unstable.waybar
          
        ];
    }) ;
}
