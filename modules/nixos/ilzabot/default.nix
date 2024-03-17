{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.ilzabot;
in {
  options.modules.ilzabot = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    virtualisation.oci-containers = {
      backend = "docker";
      containers = {
        ilzabot = {
          autoStart = true;
          image = "ilza";
        };
      };
    };
  };
}
