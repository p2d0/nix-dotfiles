{pkgs ? import <nixpkgs> {} }:

(pkgs.flac.overrideAttrs(oldAttrs: rec {
  version = "1.3.4";
  src = pkgs.fetchurl {
    url = "http://downloads.xiph.org/releases/flac/flac-${version}.tar.xz";
    # Official checksum is published at https://github.com/xiph/flac/releases/tag/${version}
    hash = "sha256-j/BgfnWjIt181uxI9PIlRxQEricw0OqUUSexNVFV5zc=";
  };
}))
