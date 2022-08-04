{ mkDerivation, pkgs, haskellPackages, lib}:

with haskellPackages;
mkDerivation {
  pname = "taffybar";
  version = "3.3.0";
  src = /etc/nixos/taffybar/taffybar;
  doHaddock = false;
  # sha256 = "17ggcv1y3md11sccbb9mpss2qdanlkv7wy098qh28gra9kq4ibgm";
  isLibrary = true;
  isExecutable = true;
  # enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    data-default http-conduit
    ansi-terminal base broadcast-chan bytestring ConfigFile containers
    dbus dbus-hslogger directory dyre either enclosed-exceptions
    filepath gi-cairo gi-cairo-connector gi-cairo-render gi-gdk
    gi-gdkpixbuf gi-gdkx11 gi-glib gi-gtk gi-gtk-hs gi-pango
    gtk-sni-tray gtk-strut haskell-gi haskell-gi-base hslogger
    HStringTemplate http-client http-client-tls http-types multimap
    old-locale parsec process rate-limit regex-compat safe scotty split
    status-notifier-item stm template-haskell text time
    time-locale-compat time-units transformers transformers-base tuple
    unix utf8-string X11 xdg-basedir xdg-desktop-entry xml xml-helpers
    xmonad
  ];
  libraryPkgconfigDepends = [ pkgs.gtk3 ];
  executableHaskellDepends = [
    base directory hslogger optparse-applicative
  ];
  executablePkgconfigDepends = [ pkgs.gtk3 ];
  description = "A desktop bar similar to xmobar, but with more GUI";
  license = lib.licenses.bsd3;
  platforms = [
    "aarch64-linux" "armv7l-linux" "i686-linux" "x86_64-linux"
  ];
  hydraPlatforms = lib.platforms.none;
  maintainers = with lib.maintainers; [ rvl ];
  broken = true;
}
