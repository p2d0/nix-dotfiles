{ appimageTools, lib, fetchurl }:

# with import <nixos-unstable> {};
appimageTools.wrapType2 rec {
  pname = "aistudio-pake";
  version = "1.0.0";

  src = /etc/nixos/appimages/aistudio.AppImage;
}
