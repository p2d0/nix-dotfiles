{ config, lib, pkgs, ... }:

{
  # displayManager = {
  #   defaultSession = "none+xmonad";
  # };

  # systemd.user.tmpfiles.rules = [
  #   "L ${config.home.homeDirectory}/.xmonad/lib - - - - /etc/nixos/modules/xmonad/lib"
  # ];

  home.file = {
    ".xmonad/lib".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/xmonad/lib;

    ".xmonad/xmonad.hs" = {
      source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/xmonad/xmonad.hs;
    };
  };
}
