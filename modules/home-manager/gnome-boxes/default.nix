{ config, lib, pkgs, ... }:

{
  home.file = {
    ".local/share/gnome-boxes".source = config.lib.file.mkOutOfStoreSymlink /mnt/md127/gnome-boxes; # TODO Second drive variable
  };
}
