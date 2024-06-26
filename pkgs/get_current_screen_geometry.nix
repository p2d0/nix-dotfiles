{ pkgs ? import <nixpkgs> {} }:

let py = pkgs.python3.withPackages(ps: [ ps.pygobject3 ]);
in
with pkgs;
stdenv.mkDerivation rec {
  pname = "get_current_screen_geometry";
  version = "1.0.0";
  src = /etc/nixos/.pythonbin/get_current_screen_geometry;
  #   unpackCmd = ''
  # '';
  dontUnpack = true;

  nativeBuildInputs = [
    gobject-introspection
    wrapGAppsHook
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
cp $src $out/bin/get_current_screen_geometry
chmod +x $out/bin/get_current_screen_geometry
wrapProgram $out/bin/get_current_screen_geometry \
--prefix PATH ":" ${lib.makeBinPath [
  py
]}
  '';
}
