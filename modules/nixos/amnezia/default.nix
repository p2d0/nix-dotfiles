{ config, lib, pkgs, inputs, ... }:

with lib;
let cfg = config.modules.amnezia;
in {
  options.modules.amnezia = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    boot.kernelPackages = pkgs.amnezia.linuxKernel.packages.linux_zen;
    boot.extraModulePackages = [config.boot.kernelPackages.amneziawg];
    environment.systemPackages = [
      pkgs.amnezia.amneziawg-tools
    ];

  };
}
