{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.keyrings;
in {
  options.modules.keyrings = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    programs.seahorse.enable = false;
    services = {
      gnome.at-spi2-core.enable = true;
    };
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };
    systemd.user.services.gnomo-polkit = {
      description = "Gnome polkit gui";
      serviceConfig = {
        ExecStart =
          "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      };
      wantedBy = [ "multiuser.target" ];
      enable = true;
    };

  };
}
