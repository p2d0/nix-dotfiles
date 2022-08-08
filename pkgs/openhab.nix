{ stdenv, lib, pkgs, ... }:

with pkgs;
stdenv.mkDerivation rec {
  pname = "openhab";
  version = "3.3.0";
  src = fetchurl {
    url = "https://openhab.jfrog.io/artifactory/libs-release-local/org/openhab/distro/openhab/${version}/openhab-${version}.zip";
    sha256 = "sha256-gCVX4ij0Vh7tlhVHPKw4IPZ+PyBCPlivBUreJy3v/hc=";
  };
  dontUnpack = true;

  nativeBuildInputs = [
    unzip
  ];

  # unpackCmd = "unzip $src";
  installPhase = ''
    mkdir p /var/lib/${pname}
    unzip $src -d /var/lib/${pname}
    chmod -R 755 /var/lib/${pname}
  '';


  meta = with lib; {
    platforms = [ "x86_64-linux" ];
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
  };
}
