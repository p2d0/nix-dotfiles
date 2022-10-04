{ mkDerivation, pkgs, haskellPackages, lib,  ... }:

with haskellPackages;
mkDerivation
  {
    pname = "my-taffybar";
    version = "0.1.0.0";
    src = ./taffybar;
    isLibrary = true;
    isExecutable = true;
    doHaddock = false;
    executableHaskellDepends = [
      base bytestring
      aeson http-client memory servant servant-client servant-client-core
      containers directory filepath gi-gdk
      (haskellPackages.callPackage ./taffy.nix { })
      gi-gtk gtk-sni-tray gtk-strut haskell-gi-base hostname hslogger
      http-types process simple-cmd split text time transformers
      X11 xdg-basedir
    ];
    executablePkgconfigDepends = [ pkgs.gtk3
                                   pkgs.noto-fonts];

    testHaskellDepends = [ base HUnit ];
    license = "unknown";
    hydraPlatforms = lib.platforms.none;
    broken = true;
  }
