{ config, lib, pkgs, ... }:

{
  home.file = {
    ".config/GIMP/2.10/scripts".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/home-manager/gimp/scripts;
    ".config/GIMP/2.10/plug-ins".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/home-manager/gimp/plug-ins;
  };
  home.file = {
    ".config/GIMP/2.10/toolrc".source = config.lib.file.mkOutOfStoreSymlink ./toolrc;
    ".config/GIMP/2.10/gimprc".source = config.lib.file.mkOutOfStoreSymlink ./gimprc;
    ".config/GIMP/2.10/menurc".source = config.lib.file.mkOutOfStoreSymlink ./menurc;
    ".config/GIMP/2.10/sessionrc".source = config.lib.file.mkOutOfStoreSymlink ./sessionrc;
  };
}
