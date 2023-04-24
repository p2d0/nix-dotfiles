{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.rofi
  ];
  home.file = {
    ".config/rofi" = {
      source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/home-manager/rofi/rofi;
    };
  };
}
