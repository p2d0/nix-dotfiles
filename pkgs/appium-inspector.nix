{ config, lib, pkgs, ... }:

# with import <nixpkgs> {};
with pkgs; appimageTools.wrapType2 { # or wrapType1
  name = "appium-inspector";


  src = fetchurl {
    url = "https://github.com/appium/appium-inspector/releases/download/v2024.3.4/Appium-Inspector-2024.3.4-linux-x86_64.AppImage";
    sha256 =  "sha256-rLDne7F9OvIFZGKzAT3ZvogfepWTwh9l5XMQ1Fh6ADQ=";
  };
  extraPkgs = pkgs: with pkgs; [ ];


}
