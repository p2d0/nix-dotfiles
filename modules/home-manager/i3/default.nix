{ config, lib, pkgs, ... }:

{
  home.file = {
    ".config/i3".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/i3;

  };
}
