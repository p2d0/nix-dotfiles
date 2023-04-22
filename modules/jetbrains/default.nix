{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.jetrbains;
    unstable = import <nixos-unstable> { config.allowBroken = true; config.allowUnfree = true; };
in {
  options.modules.jetrbains = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
    enableAndroidStudio = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };

  };
  config = mkIf cfg.enable (lib.my.withHome {
    home.file = {
      ".ideavimrc" = {
        source = ./configs/ideavim/.ideavimrc;
      };

      ".intellimacs" = {
        source = ./configs/ideavim/.intellimacs;
        recursive = true;
      };
    };
  }
    {
      environment.systemPackages = with pkgs;
        [
          unstable.jetbrains.idea-community
        ];

    }
  );
}
