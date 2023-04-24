{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.xmonad;
in {
  options.modules.xmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      Enable xmonad with configs
      '';
    };
  };
  config = mkIf cfg.enable {
    services.xserver = {
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      displayManager = { defaultSession = "none+xmonad"; };
    };
  } // (my.allUsers ({}: {
    home.file = {
      ".xmonad/lib".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/nixos/xmonad/lib;

      ".xmonad/xmonad.hs" = {
        source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/nixos/xmonad/xmonad.hs;
      };
    };
  }));
}


  # {
  #   # displayManager = {
  #   #   defaultSession = "none+xmonad";
  #   # };

  #   # systemd.user.tmpfiles.rules = [
  #   #   "L ${config.home.homeDirectory}/.xmonad/lib - - - - /etc/nixos/modules/nixos/xmonad/lib"
  #   # ];

  # }
