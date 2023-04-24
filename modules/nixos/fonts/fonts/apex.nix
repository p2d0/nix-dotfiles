{ pkgs }:

with pkgs;
stdenv.mkDerivation {
  pname = "apex";
  version = "1.0.0";
  src = [
    ./apex-ExtraLight.otf
    ./apex-Medium.otf
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

  meta = with lib; {
    description = "TrueType font for Lao language";
    license = licenses.gpl2Plus;
    maintainers = with lib.maintainers; [ serge ];
    platforms = platforms.all;
  };
}
