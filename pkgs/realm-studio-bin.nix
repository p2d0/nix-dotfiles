{ appimageTools, lib, fetchurl, pkgs  }:

# with import <nixos-unstable> {};
appimageTools.wrapType2 rec {
  pname = "realm-studio-bin";
  version = "20.0.0";

  src = fetchurl {
    url = "https://github.com/realm/realm-studio/releases/download/v20.0.0/Realm.Studio-20.0.0.AppImage";
    sha256 = "sha256-dAQ0ZbY8X18vRgm52HbjeQ1QKCASByAJkcCnFSy/uxQ=";
  };
  extraPkgs = pkgs: with pkgs; [openssl curl];
}
