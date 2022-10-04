{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.rofi
  ];
  home.file = {
    ".config/rofi" = {
      source = ./rofi;
      recursive = true;
    };
  };
}
