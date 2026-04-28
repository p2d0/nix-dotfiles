{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.appblock;
in {
  options.modules.appblock = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        Enable appblocker service and symlink config
      '';
    };
  };
  config = mkIf cfg.enable {
    services.appblocker.enable = true;

    home-manager.users.${config.user} =
      { config, ... }: {
        home.file = {
          ".config/appblocker/config.toml" = {
            source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/appblocker/config.toml;
          };
        };
      };
  };
}
