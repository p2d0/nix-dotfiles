{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.gimp3-photoshop-shortcuts;
in {
  options.modules.gimp3-photoshop-shortcuts = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {

    home.file = {
      ".config/GIMP/3.0/shortcutsrc" = {
        source = ./shortcutsrc;
        force = true;
      };
      ".config/GIMP/3.0/controllerrc" = {
        source = ./controllerrc;
        force = true;
      };
    };
  };
}
