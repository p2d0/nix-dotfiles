{ pkgs ? import <nixpkgs> {} }:

let py = pkgs.python3.withPackages(ps: [ ps.pygobject3 ps.i3-py ]);
in
with pkgs;
stdenv.mkDerivation rec {
  pname = "get_current_screen_geometry";
  version = "1.0.0";
  src = /etc/nixos/.pythonbin;
  #   unpackCmd = ''
  # '';
  dontUnpack = true;

  nativeBuildInputs = [
    gobject-introspection
    wrapGAppsHook3
    makeWrapper
  ];

  buildInputs = [
    gtk3
    py
  ];

  runtimeDependencies = with python3.pkgs; [
    pygobject3
  ];

  propagatedBuildInputs = with python3.pkgs; [
    pygobject3
  ];
  installPhase = ''
mkdir -p $out/bin
cp $src/* $out/bin/
chmod +x $out/bin/*
for file in $out/bin/*
do
wrapProgram $file \
--prefix PATH ":" ${lib.makeBinPath [
  py
]}
done
  '';
}
