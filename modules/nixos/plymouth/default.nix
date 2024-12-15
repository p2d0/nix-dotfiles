{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.plymouth;
in {
  options.modules.plymouth = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    boot = {
      plymouth = {
        enable = true;
        theme = "dark_planet";
        themePackages = with pkgs; [
          # By default we would install all themes
          (adi1090x-plymouth-themes.override {
            selected_themes = [ "circuit" "dark_planet" ];
          })
        ];
      };
    };
  };
}
