{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.vm;
in {
  options.modules.vm = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    virtualisation.spiceUSBRedirection.enable = true;
    virtualisation.libvirtd.enable = true;
    users.users.${config.user}.extraGroups = [ "libvirtd" ];
    environment.systemPackages = with pkgs;
      [
        looking-glass-client
        virt-manager
      ];
  };
}
