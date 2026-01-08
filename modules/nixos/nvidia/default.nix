{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.nvidia;
in {
  options.modules.nvidia = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    environment = {
      etc."nvidia/nvidia-application-profiles-rc.d/50-limit-free-buffer-pool.json".text = builtins.toJSON {
        rules =
          map (proc: {
            pattern = {
              feature = "procname";
              matches = proc;
            };
            profile = "No VidMem Reuse";
          }) [
            "Hyprland"
            ".Hyprland-wrapped"
            "firefox"
            ".firefox-wrapped"
            "Brave"
            "brave"
            ".brave-wrapped"
            "Discord"
            ".Discord-wrapped"
            "DiscordCanary"
            ".DiscordCanary-wrapped"
            "electron"
            ".electron-wrapped"
            "librewolf"
            ".librewolf-wrapped"
            "losslesscut"
            ".losslesscut-wrapped"
          ];
      };
    };
  };
}

