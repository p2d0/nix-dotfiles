{ pkgs }:

with pkgs;
stdenv.mkDerivation {
  pname = "sfpro";
  version = "1.0.0";
  src = [
    ./sfprodisplaybold.otf
    ./sfprodisplayregular.otf
  ];
  dontUnpack = true;

  # https://discourse.nixos.org/t/mkderivation-src-as-list-of-filenames/3537/10
  # striping hash multi src
  installPhase = ''
    mkdir -p $out/share/fonts
    for srcFile in $src; do
      cp $srcFile $out/share/fonts/$(stripHash $srcFile)
    done
  '';
}
