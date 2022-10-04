{ python3, stdenv, lib, pkgs, ... }:

with pkgs;
stdenv.mkDerivation rec {
  pname = "get_current_screen_geometry";
  version = "1.0.0";
  src = /etc/nixos/.pythonbin/get_current_screen_geometry;
  unpackCmd = "";
  # dontUnpack = true;

  nativeBuildInputs = [
    gobject-introspection
    makeWrapper
  ];

  buildInputs = [
    gtk3
    python3
  ];

  propagatedBuildInputs = with python3.pkgs; [
    pygobject3
  ];
  installPhase = ''
echo $src;
mkdir -p $out/bin
cp $src $out/bin
ls -al;
chmod +x $out/bin/get_current_screen_geometry
wrapProgram $out/bin/get_current_screen_geometry \
--prefix PATH ":" ${lib.makeBinPath [
  python3
]}
  '';
}
