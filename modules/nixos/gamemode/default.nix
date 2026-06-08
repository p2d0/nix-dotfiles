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
    users.users.andrew.extraGroups = ["corectrl" "gamemode"];
    # programs.corectrl ={
    #   enable = true;
    #   gpuOverclock.enable = true;
    # };
    programs.gamemode = {
      enable = true;
      settings = {
        general = {
          reaper_freq = 5;
          desiredgov = "performance";
          
          # Disables iGPU/CPU power balancing checks since you are on a dedicated setup
          igpu_power_threshold = "-1";
          
          # Uses SCHED_ISO if supported by your kernel on 4+ cores
          softrealtime = "auto";
          
          # Renices the game process for higher scheduling priority
          renice = 10;
          ioprio = 0;
          inhibit_screensaver = 1;
          disable_splitlock = 1;
        };

        cpu = {
          # The Ryzen 3600 has a single uniform cache pool per core.
          # No need to park cores, but pinning keeps threads from hopping.
          park_cores = "no";
          pin_cores = "yes";
        };

        # Warning: GPU optimisations have the potential to damage hardware
        # gpu = {
        #   apply_gpu_optimisations = "accept-responsibility";
        #   gpu_device = 0;
        #   amd_performance_level = "high";
        # };
      };
    };
  };
}
