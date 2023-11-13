{ appimageTools, lib, fetchurl }:

# with import <nixos-unstable> {};
appimageTools.wrapType2 rec {
  pname = "cursor";
  version = "0.8.4";

  src = fetchurl {
    url = "https://dl.todesktop.com/230313mzl4w4u92/linux/appImage/x64";
    sha256 = "";
  };
}
