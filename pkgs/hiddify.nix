# { appimageTools, lib, fetchurl, makeDesktopItem }:

with import <nixpkgs> {};
appimageTools.wrapType2 rec {
  pname = "hiddify";
  version = "2.5.71";

  extraPkgs = pkgs: with pkgs; [
    libepoxy
  ];

  src = fetchurl {
    url = "https://github.com/hiddify/hiddify-next/releases/download/v2.5.7/Hiddify-Linux-x64.AppImage";
    sha256 = "sha256-5RqZ6eyurRtoOVTBLZqoC+ANi4vMODjlBWf3V4GXtMg=";
  };

  desktopItems = [(makeDesktopItem {
    name = pname;
    exec = "hiddify";
    desktopName = "Hiddify";
    icon = pname;
    comment = "VPN";
    categories = ["VPN"];
  })];
}
