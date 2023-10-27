{ lib
, libGL
, libX11
, pkgs
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
}:

# with import <nixos-unstable> {};
# with qt5;
# with xorg;
stdenv.mkDerivation rec {
  pname = "natron";
  version = "2.5.0";

  src = builtins.fetchurl {
    url = "https://github.com/NatronGitHub/Natron/releases/download/v${version}/Natron-${version}-Linux-x86_64-no-installer.tar.xz";
  };
  # unpackCmd = "tar -xf $curSrc -C .";

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = [
    ffmpeg-full
    openfx
    openimageio
    boost
    expat
    python310
    python310.pkgs.pyside2
    python310.pkgs.shiboken2
    wayland
    glog
    ceres-solver
    libXdmcp
    libXrender
    libXrandr
    libXi
    libxcrypt
    libxcrypt-legacy
    libXcursor
    libXinerama
    libGLU
    alsa-lib
    ncurses
    readline
  ];
  libpath = lib.makeLibraryPath buildInputs;

  installPhase = ''
    mkdir -p $out/bin
    cp -r "./"* "$out"
    # #cp $out/Telegram $out/share/telegram
    # ls -al bin/
    # chmod +x Natron
    wrapProgram \
      $out/Natron \
      --prefix LD_LIBRARY_PATH : "${libpath}"
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
