{ config, lib, pkgs, ... }:

with pkgs;
stdenv.mkDerivation rec {
  pname = "puush-linux";
  version = "1.0.0";
  src = fetchFromGitHub {
    owner = "NickHu";
    repo = "puush-linux";
    rev = "master";
    sha256 = "7wpIXJ4zaI0dYuGGVOzStfZ4ZXE7kVGVVgpg3/TPznY=";
  };
  # unpackCmd = "";
  # dontUnpack = true;

  nativeBuildInputs = [
    makeWrapper
  ];

  installPhase = ''
echo $src;
mkdir -p $out/bin
cp $src/* $out/bin
ls -al;
chmod +x $out/bin/puush'';
}
