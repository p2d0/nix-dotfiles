{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.xdg;
in {
  options.modules.xdg = {
    sessionVariables = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.sessionVariables {
    environment.sessionVariables = {
      GTK_DATA_PREFIX = [ "${config.system.path}" ];
      XDG_CACHE_HOME  = "$HOME/.cache";
      XDG_CONFIG_HOME = "$HOME/.config";
      XDG_DATA_HOME   = "$HOME/.local/share";
      XDG_STATE_HOME  = "$HOME/.local/state";
    };
  };
}
