{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.gamemode;
in {
  options.modules.gamemode = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    users.groups.gamemode = {};
    users.users.andrew.extraGroups = ["corectrl", "gamemode"];
    programs.corectrl ={
      enable = true;
      gpuOverclock.enable = true;
    };
    programs.gamemode = {
      enable = true;
      settings = {
        general = {
          renice = 10;
        };

        # Warning: GPU optimisations have the potential to damage hardware
        gpu = {
          apply_gpu_optimisations = "accept-responsibility";
          gpu_device = 0;
          amd_performance_level = "high";
        };
      };
    };
  };
}
