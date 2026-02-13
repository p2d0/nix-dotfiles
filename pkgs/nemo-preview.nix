{ stdenv
, lib
, fetchFromGitHub
, meson
, pkg-config
, ninja
, glib
, gtk3
, callPackage
, gnome
, gobject-introspection
, wrapGAppsHook3
, clutter
, clutter-gtk
, clutter-gst
, cinnamon
, gtksourceview4
, libmusicbrainz5
, webkitgtk_4_1
}:

# with import <nixos-unstable> {};
with cinnamon;
let
  xreader = callPackage ./xreader.nix { };
in
stdenv.mkDerivation rec {
  pname = "nemo-preview";
  version = "5.8.0";

  src = fetchFromGitHub {
    owner = "linuxmint";
    repo = "nemo-extensions";
    rev = version;
    sha256 = "sha256-tyRYPWJa93w05a0PcYvz1GA8/xX2kHLdIzz4tCcppiY=";
  };
  # patches = [
  #   ./nemo-preview.patch
  # ];

  sourceRoot = "${src.name}/nemo-preview";

  nativeBuildInputs = [
    meson
    pkg-config
    ninja
    xreader
    gobject-introspection
    clutter
    clutter-gtk
    clutter-gst
    cinnamon.cjs
    gtksourceview4
    libmusicbrainz5
    webkitgtk_4_1
  ];

  buildInputs = [
    glib
    gtk3
    nemo
  ];

  runtimeDependencies = [
    clutter
    clutter-gtk
    clutter-gst
    xreader
    cinnamon.cjs
    gtksourceview4
  ];

  # postPatch = ''
  #   substituteInPlace src/nemo-preview.c \
  #     --replace "preview" "${lib.getExe gnome.preview}"
  # '';

  PKG_CONFIG_LIBNEMO_EXTENSION_EXTENSIONDIR = "${placeholder "out"}/${nemo.extensiondir}";

  meta = with lib; {
    homepage = "https://github.com/linuxmint/nemo-extensions/tree/master/nemo-preview";
    description = "Nemo file roller extension";
    license = licenses.gpl2Plus;
    platforms = platforms.linux;
    maintainers = teams.cinnamon.members;
  };

}
