{ pkgs }:

with pkgs;
stdenv.mkDerivation rec {
  pname = "cryptocoins";
  version = "1.0.1";
  src = fetchFromGitHub {
    owner = "AllienWorks";
    repo = pname;
    rev = "master";
    hash = "sha256-IR/7doO7qR+3UJ3WBsvna5LR6aH5TqNtYdlUbQXcsRc=";
  };

  # dontUnpack = true;

  installPhase = ''
    mkdir -p $out/share/fonts
    cp $src/webfont/cryptocoins.ttf $out/share/fonts/cryptocoins.ttf
    cp $src/webfont/cryptocoins.woff $out/share/fonts/cryptocoins.woff
    cp $src/webfont/cryptocoins.woff2 $out/share/fonts/cryptocoins.woff2
'';
}
