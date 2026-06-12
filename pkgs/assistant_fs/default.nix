{ appimageTools, lib, pkgs }:

appimageTools.wrapType2 rec {
  pname = "assistant-fs";
  version = "6.5-1";

  src = ./assistant_fs-6.5-1_x86_64;

  extraPkgs = pkgs: with pkgs; [ gtk2 sqlite xorg.libxcb ];
}
