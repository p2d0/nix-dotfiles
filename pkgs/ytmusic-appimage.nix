{ appimageTools, lib, fetchurl, pkgs }:

# with import <nixos-unstable> {};
appimageTools.wrapType2 rec {
  pname = "ytmusic-appimage";
  version = "1.0.0";

  src = fetchurl {
    url = "https://github.com/tw93/Pake/releases/download/V3.1.1/YouTubeMusic_x86_64.AppImage";
    sha256 = "sha256-ZRedPl44fR+xEtP+Ufnl55SCwB1uFqciutmEc6Hloto=";
  };
  extraPkgs = pkgs: with pkgs; [pkgs.openssl_3_2Pkgs.openssl_3_2];
}
