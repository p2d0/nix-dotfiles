{ unzip
, xorg
, lib
, stdenv
, fetchurl
, oraclejdk8
, makeDesktopItem
, libpulseaudio
, libGL
}:

# with import <nixos-unstable> {};
stdenv.mkDerivation rec {
  pname = "tlauncher";
  version = "1.0.2";

  src = fetchurl {
    url = "https://tlauncher.org/jar";
    sha256 = "sha256-rhI5gil/U1LFeOs/zESLNi9n98CFaKgvbRjQXEIlhNg=";
  };
  nativeBuildInputs = [
    unzip
  ];

  libpath = with xorg; lib.makeLibraryPath [
    libX11
    libXext
    libXcursor
    libXrandr
    libXxf86vm
    libpulseaudio
    libGL
  ];

  buildCommand = ''
  mkdir $out
  mkdir $out/bin;
  unzip -d $out/bin $src
  touch $out/bin/tlauncher
  cat > $out/bin/tlauncher << EOF
   #!/bin/sh
   export LD_LIBRARY_PATH="$libpath"
   export JAVA_HOME="${oraclejdk8}"
   exec ${oraclejdk8}/bin/java \$@ -jar $out/bin/TLauncher-2.86.jar
  EOF
  chmod +x $out/bin/tlauncher
  '';

  desktopItems = makeDesktopItem {
    name = pname;
    exec = "tlauncher";
    icon = "tlauncher";
    comment = meta.description;
    desktopName = "Tlauncher";
    genericName = "Tlauncher";  };

  meta = with lib; {
    description = "Tlauncher minecraft";
    homepage = "https://tlauncher.org/";
  };
}
