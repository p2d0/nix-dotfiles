{ pkgs }:

with pkgs;

stdenv.mkDerivation {
  pname = "bell-and-lamb";
  version = "1.0.0";
  src = ./bell-and-lamb.otf;
  dontUnpack = true;
  installPhase = ''
    mkdir -p $out/share/fonts
    cp $src $out/share/fonts/bell-and-lamb.otf'';

  meta = with lib; {
    description = "TrueType font for Lao language";
    license = licenses.gpl2Plus;
    maintainers = with lib.maintainers; [ serge ];
    platforms = platforms.all;
  };
}
