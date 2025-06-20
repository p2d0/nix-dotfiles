{ appimageTools, lib, fetchurl, pkgs  }:

# with import <nixos-unstable> {};
appimageTools.wrapType2 rec {
  pname = "grok-appimage";
  version = "1.0.0";

  src = fetchurl {
    url = "https://axiom.ai/desktop_app/axiom-desktop-linux-latest.AppImage";
    sha256 = "sha256-sw2Q/LtJKfoW2mUx2rya2l6OFqHrKPVw2BDbk1+Ejn4=";
  };
  extraPkgs = pkgs: with pkgs; [openssl curl];
}
