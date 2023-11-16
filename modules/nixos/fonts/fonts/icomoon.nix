{ pkgs }:

with pkgs;
stdenv.mkDerivation {
  pname = "icomoon";
  version = "1.0.0";
  src = ./Icomoon-Feather.ttf;
  dontUnpack = true;
  installPhase = ''
    mkdir -p $out/share/fonts
    cp $src $out/share/fonts/Icomoon-Feather.ttf'';
}
