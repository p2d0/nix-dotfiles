{ config, lib, pkgs, ... }:

{
  home.file = {
    ".config/polybar".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/polybar;
  };
  home.packages = [
    pkgs.polybarFull
  ];
}
