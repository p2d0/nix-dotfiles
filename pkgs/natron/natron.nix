{ lib
, stdenv
, fetchFromGitHub
, cmake
, pkg-config
, wrapQtAppsHook
, boost
, cairo
, ceres-solver
, expat
, extra-cmake-modules
, glog
, libXdmcp
, python310
, wayland
}:

# with import <nixos-unstable> {};
# with pkgs.xorg;
# with pkgs.qt5;
let
  minorVersion = "2.5";
  version = "${minorVersion}.0";
  OpenColorIO-Configs = fetchFromGitHub {
    owner = "NatronGitHub";
    repo = "OpenColorIO-Configs";
    rev = "Natron-v${minorVersion}";
    hash = "sha256-TD7Uge9kKbFxOmOCn+TSQovnKTmFS3uERTu5lmZFHbc=";
  };
in
stdenv.mkDerivation {
  inherit version;
  pname = "natron";

  src = fetchFromGitHub {
    owner = "NatronGitHub";
    repo = "Natron";
    rev = "v${version}";
    fetchSubmodules = true;
    hash = "sha256-dgScbfyulZPlrngqSw7xwipldoRd8uFO8VP9mlJyhQ8=";
  };

  cmakeFlags = [ "-DNATRON_SYSTEM_LIBS=ON" ];

  nativeBuildInputs = [
    cmake
    pkg-config
    wrapQtAppsHook
  ];

  buildInputs = [
    boost
    expat
    cairo
    python310
    python310.pkgs.pyside2
    python310.pkgs.shiboken2
    extra-cmake-modules
    wayland
    glog
    ceres-solver
    # openimageio
    libXdmcp
  ];

  runtimeDependencies = [
    # openimageio
  ];

  postInstall = ''
    mkdir -p $out/share
    cp -r ${OpenColorIO-Configs} $out/share/OpenColorIO-Configs
  '';

  postFixup = ''
    wrapProgram $out/bin/Natron \
      --prefix PYTHONPATH : "${python310.pkgs.makePythonPath [ python310.pkgs.qtpy python310.pkgs.pyside2 ]}" \
      --set-default OCIO "$out/share/OpenColorIO-Configs/blender/config.ocio"
  '';

  meta = with lib; {
    description = "Node-graph based, open-source compositing software";
    longDescription = ''
      Node-graph based, open-source compositing software. Similar in
      functionalities to Adobe After Effects and Nuke by The Foundry.
    '';
    homepage = "https://natron.fr/";
    license = lib.licenses.gpl2;
    maintainers = [ maintainers.puffnfresh ];
    platforms = platforms.linux;
    broken = stdenv.isLinux && stdenv.isAarch64;
  };
}
