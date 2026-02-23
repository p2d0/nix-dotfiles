{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.corectrl;
in {
  options.modules.corectrl = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    security.polkit.extraConfig = ''
polkit.addRule(function(action, subject) {
    if (action.id == "org.corectrl.helper.init" &&
        subject.user == "${config.user}") {
        return polkit.Result.YES;
        }
});

polkit.addRule(function(action, subject) {
    if (action.id == "org.corectrl.helper.init" &&
        subject.user == "${config.user}") {
        return polkit.Result.YES;
        }
});
'';
    programs.corectrl ={
      enable = true;
    };

    users.users.andrew.extraGroups = ["corectrl" "gamemode"];
  };
}
