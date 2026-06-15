{ lib
, libGL
, libX11
, pkgs
, makeDesktopItem
, stdenv
, autoPatchelfHook
, makeWrapper
, ffmpeg-full
, openfx
, openimageio
, boost
, expat
, python310
, wayland
, glog
, ceres-solver
, libXdmcp
, libXrender
, libXrandr
, libXi
, libxcrypt
, libxcrypt-legacy
, libXcursor
, libXinerama
, libGLU
, alsa-lib
, ncurses
, readline
, copyDesktopItems
}:

with import <nixpkgs> {};
# with qt5;
# with xorg;
stdenv.mkDerivation rec {
  pname = "natron";
  version = "2.5.0";

  src = builtins.fetchurl {
    url = "https://github.com/NatronGitHub/Natron/releases/download/v${version}/Natron-${version}-Linux-x86_64-no-installer.tar.xz";
  };
  # unpackCmd = "tar -xf $curSrc -C .";

  nativeBuildInputs = [ autoPatchelfHook qt5.wrapQtAppsHook pkg-config makeWrapper copyDesktopItems ];

  buildInputs = [
    pkgs.libxcrypt-legacy
    pkgs.libGLU
    pkgs.alsa-lib
    pkgs.ncurses
    pkgs.readline
    pkgs.libGL
    pkgs.xorg.libX11
    pkgs.xorg.libXrender
    pkgs.libgcc.lib
    pkgs.xorg.libSM
    pkgs.xorg.libICE
    pkgs.xorg.libXext
    pkgs.xorg.libXi
    pkgs.xorg.libXrandr
    pkgs.xorg.libXfixes
    pkgs.xorg.libXcursor
    pkgs.xorg.libXinerama
  ];

  # buildInputs = [
  #   ffmpeg-full
  #   openfx
  #   openimageio
  #   boost
  #   expat
  #   python310
  #   python310.pkgs.pyside2
  #   python310.pkgs.shiboken2
  #   wayland
  #   glog
  #   ceres-solver
  #   libXdmcp
  #   libXrender
  #   libXrandr
  #   libXi
  #   libxcrypt
  #   libxcrypt-legacy
  #   libXcursor
  #   libXinerama
  #   libGLU
  #   libGLU
  #   libGL
  #   pkgs.xorg.libX11
  #   pkgs.xorg.libXrender
  #   pkgs.libgcc.lib
  #   pkgs.xorg.libSM
  #   pkgs.xorg.libICE
  #   pkgs.xorg.libXext
  #   pkgs.xorg.libXi
  #   pkgs.xorg.libXrandr
  #   pkgs.xorg.libXfixes
  #   alsa-lib
  #   ncurses
  #   readline
  # ];

  NIX_LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
    pkgs.libxcrypt-legacy
    pkgs.libGLU
    pkgs.libGL
    pkgs.xorg.libX11
    pkgs.xorg.libXrender
    pkgs.libgcc.lib
    pkgs.xorg.libSM
    pkgs.xorg.libICE
    pkgs.xorg.libXext
    pkgs.xorg.libXi
    pkgs.xorg.libXrandr
    pkgs.xorg.libXfixes
    pkgs.xorg.libXcursor
    pkgs.xorg.libXinerama
  ];

  desktopItems = [(makeDesktopItem {
    name = pname;
    exec = "Natron";
    desktopName = "Natron";
    icon = pname;
    comment = "Video editing software";
    categories = ["Video"];
  })];

  installPhase = ''
    mkdir -p $out/bin
    cp -r "./"* "$out"
    for i in 16 32 48 64 96 128 256 512 1024; do
      install -D ./Resources/pixmaps/natronIcon256_linux.png $out/share/icons/hicolor/''${i}x$i/apps/natron.png
    done
    # wrapProgram \
    #   $out/bin/Natron \
    #   --prefix LD_LIBRARY_PATH : "${NIX_LD_LIBRARY_PATH}" \
    #   --prefix BUNDLE_LIBRARY_PATH : "$out/lib"
    runHook postInstall
  '';

}


  # pkgs.tdesktop.overrideAttrs (oldAttrs: rec {
  #   version = "4.2.0";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "telegramdesktop";
  #     repo = "tdesktop";
  #     rev = "v${version}";
  #     fetchSubmodules = true;
  #     sha256 = "07fhm36394171w0rvay1x9x1br3z36z4dlpi57bkq23dvi331pxj";
  #   };
  # })
