{ lib
, stdenv
, autoPatchelfHook
, makeWrapper
, jetbrains
, zlib
, glib
, libX11
, libXi
, libXrender
, freetype
, alsa-lib
, libXtst
, jdk
}:

stdenv.mkDerivation rec {
  pname = "jetbrains-gateway";
  version = "222.2270.16";

  src = builtins.fetchurl {
    url = "https://download.jetbrains.com/idea/gateway/JetBrainsGateway-222.2270.16.tar.gz";
  };

  nativeBuildInputs = [ autoPatchelfHook makeWrapper ];
  buildInputs = [
    jetbrains.jdk
    zlib
    glib
    libX11
    libXi
    libXrender
    freetype
    alsa-lib
    libXtst
  ];

  installPhase = ''
    mkdir -p $out/share
    cp -r . $out/share
    rm -r $out/share/jbr

    makeWrapper \
      $out/share/bin/gateway.sh \
      $out/bin/jetbrains-gateway \
      --prefix LD_LIBRARY_PATH : $out/lib \
      --set GATEWAY_JDK "${jdk}" \
      --set JAVA_OPTS "-Xms512m, -Xmx2048m" \
      --set JETBRAINSCLIENT_JDK "${jdk}"
  '';
}
