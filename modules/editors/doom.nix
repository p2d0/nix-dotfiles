{ config, lib, pkgs, ... }:

{
  home.file = {
     ".doom.d".source = config.lib.file.mkOutOfStoreSymlink ./.doom.d;
  };
}
