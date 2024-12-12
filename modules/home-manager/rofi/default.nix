{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.rofi-wayland
  ];
  home.file = {
    ".config/rofi" = {
      source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/home-manager/rofi/rofi;
    };
  };
}
