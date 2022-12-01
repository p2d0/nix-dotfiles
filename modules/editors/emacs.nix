{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.emacs-with-doom;
in {
  options.modules.emacs-with-doom = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
    emacs-dir = mkOption {
      type = types.path;
    };

  };
  config = mkIf cfg.enable {
    home.file = {
      "${config.home.homeDirectory}/.doom.d".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/editors/.doom.d;
      # ".emacs.d".source = config.lib.file.mkOutOfStoreSymlink cfg.emacs-dir;
    };
  };
}
