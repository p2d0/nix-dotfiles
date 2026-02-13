{
  libressl,
  openssl,
  stdenv,
  dpkg,
  makeWrapper,
  autoPatchelfHook,
  wrapGAppsHook3,
  gtk3,
  cairo,
  gdk-pixbuf,
  gobject-introspection,
  glib,
  webkitgtk,
  gcc,
  libappindicator-gtk3,
  glib-networking
}:

stdenv.mkDerivation rec {
  pname = "ChatGPT";
  version = "0.12.0";

  src = builtins.fetchurl {
    url = "https://github.com/lencx/ChatGPT/releases/download/v${version}/ChatGPT_${version}_linux_x86_64.deb";
    sha256 = "sha256:1vwcmzz27c52n0xzm4i3zgb9z65m9vxd27gp6q9gh8sqnnldm86q";
  };
  unpackCmd = "${dpkg}/bin/dpkg-deb -x $curSrc .";

  installPhase = ''
    mkdir -p $out
    cp -r . $out/
'';

  nativeBuildInputs = [ makeWrapper autoPatchelfHook wrapGAppsHook3 ];
  runtimeDependencies = [
    gtk3
    cairo
    gdk-pixbuf
    gobject-introspection
    glib
    gcc
    webkitgtk
    libappindicator-gtk3
    glib-networking
    libressl
    openssl
  ];

  buildInputs = [
    gtk3
    cairo
    gdk-pixbuf
    gobject-introspection
    glib
    gcc
    webkitgtk
    libappindicator-gtk3
    glib-networking
    libressl
    openssl
  ];

}
