{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.gpu-screen-recorder;
in {
  options.modules.gpu-screen-recorder = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    # systemd.services.gsr-kms-server = {
    #   enable = true;
    #   description = "GPU screen recorder service";
    #   wantedBy = [ "default.target" ];
    #   serviceConfig = {
    #     ExecStart = "${pkgs.my.gpu-screen-recorder}/bin/gsr-kms-server";
    #   };
    # };
    security.polkit.extraConfig = ''
polkit.addRule(function(action, subject) {
    if (action.id == "org.freedesktop.policykit.exec" &&
        subject.user == "${config.user}") {
        return polkit.Result.YES;
        }
});
'';

    environment.systemPackages = [
      pkgs.my.gpu-screen-recorder
    ];
  };
}
