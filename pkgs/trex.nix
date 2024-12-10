{pkgs, fetchurl, makeWrapper, stdenv, lib, fetchFromGitHub }:

# with import <nixpkgs> {config.allowUnfree = true; };
stdenv.mkDerivation rec {
  name = "trex";
  version = "0.26.9";

  src = fetchurl {
    url = "https://github.com/trexminer/T-Rex/releases/download/0.26.8/t-rex-0.26.8-linux.tar.gz";
    hash = "sha256-fncGSki0yMuNR5fzCkG1Pvu4MR/BRHW1ao5oea0cBWk=";
  };

  nativeBuildInputs = [ makeWrapper ];
  # buildInputs = [ libtool curl ncurses ocl-icd opencl-headers stdenv.cc.cc.lib
  #                 xorg.libX11 xorg.libXext xorg.libXinerama jansson libusb1 ];

  # unpackCmd = "tar -xf $curSrc -C .";
  installPhase = ''
mkdir -p $out;
mv ../* $out/
makeWrapper $out/t-rex $out/bin/t-rex \
--prefix LD_LIBRARY_PATH : "${pkgs.amnezia.linuxPackages.nvidia_x11}/lib"
'';

}
