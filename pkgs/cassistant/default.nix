{ buildFHSEnv, dpkg, gtk2, glib, gdk-pixbuf, pango, cairo, atk, dbus, libX11
, libXi, libXext, libXfixes, libXtst, sqlite, xorg, lib, stdenv, fetchurl
, makeDesktopItem, symlinkJoin }:

let
  extracted = stdenv.mkDerivation {
    pname = "cassistant-extracted";
    version = "6.5-1";

    src = fetchurl {
      url =
        "https://lk2.xn--80akicokc0aablc.xn--p1ai/WebApi/Platforms/Download/1379";
      hash = "sha256-d+AUajp2fZ33464i1czqYKN+srkhC3wvgw8YZfoWbm8=";
    };

    nativeBuildInputs = [ dpkg ];

    dontUnpack = true;

    installPhase = ''
      dpkg-deb --extract $src $out
    '';

    meta.license = lib.licenses.unfree;
  };

  fhsEnv = buildFHSEnv {
    pname = "cassistant";
    version = "6.5-1";

    targetPkgs = pkgs: [
      gtk2
      glib
      gdk-pixbuf
      pango
      cairo
      atk
      dbus
      libX11
      libXi
      libXext
      libXfixes
      libXtst
      sqlite
      xorg.libxcb
      pkgs.bash
    ];

    profile = ''
      export INST_DIR=${extracted}/opt/assistant
      export PATH=$INST_DIR/bin:$PATH
    '';

    runScript = "${extracted}/opt/assistant/bin/assistant";
  };

  desktopItem = makeDesktopItem {
    name = "cassistant";
    desktopName = "Ассистент";
    comment = "Remote desktop and administration tool";
    exec = "${fhsEnv}/bin/cassistant";
    icon = "assistant";
    terminal = false;
    type = "Application";
    categories = [ "Network" ];
  };
in symlinkJoin {
  name = "cassistant-6.5-1";
  paths = [
    fhsEnv
    (stdenv.mkDerivation {
      pname = "cassistant-desktop";
      version = "6.5-1";
      dontUnpack = true;
      installPhase = ''
        runHook preInstall

        mkdir -p $out/share/icons/hicolor/256x256/apps
        cp ${extracted}/opt/assistant/share/icons/assistant.png \
           $out/share/icons/hicolor/256x256/apps/assistant.png

        mkdir -p $out/share/applications
        cp ${desktopItem}/share/applications/cassistant.desktop $out/share/applications/

        runHook postInstall
      '';
    })
  ];

  meta = {
    description = "Ассистент - remote desktop and administration tool";
    homepage = "https://xn--80akicokc0aablc.xn--p1ai/";
    license = lib.licenses.unfree;
    platforms = [ "x86_64-linux" ];
  };
}
