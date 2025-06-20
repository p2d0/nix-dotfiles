# { config, lib, pkgs, ... }:

with import <nixpkgs> {};
pkgs.stdenv.mkDerivation rec{
  src = /etc/nixos/appimages/ytmusic.deb;
  pname = "ytmusic-deb";
  version = "1.0.0";

  nativeBuildInputs = [
    patchelf
    makeWrapper
    dpkg
  ];

  buildInputs = with pkgs;[
    libappindicator-gtk3
    libayatana-appindicator
    libayatana-appindicator-gtk3
    at-spi2-atk
    atkmm
    cairo
    gdk-pixbuf
    glib
    gtk3
    harfbuzz
    librsvg
    libsoup_3
    pango
    webkitgtk_4_1
    openssl
  ];

  GST_PLUGIN_PATH = lib.makeSearchPathOutput "lib" "lib/gstreamer-1.0" [
    pkgs.gst_all_1.gst-plugins-base
    pkgs.gst_all_1.gst-plugins-good
    pkgs.gst_all_1.gst-plugins-bad
    pkgs.gst_all_1.gst-libav
    pkgs.pipewire
  ];

  installPhase = ''
    mkdir -p $out
mv usr/* $out/
wrapProgram $out/bin/pake \
--prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath buildInputs}" \
--prefix GIO_MODULE_DIR : "${glib-networking}/lib/gio/modules/" \
--prefix GST_PLUGIN_PATH : "${GST_PLUGIN_PATH}"
'';
}
