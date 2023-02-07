{ config, lib, pkgs, ... }:

{
  home.file = {
    ".config/qtile".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/qtile;

  };
}
