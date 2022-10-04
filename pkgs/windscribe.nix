{ lib
, stdenv
, autoPatchelfHook
, makeWrapper
, pkgs
}:

with pkgs;
stdenv.mkDerivation rec {
  pname = "windscribe";
  version = "1.0";

  src = builtins.fetchurl {
    url = "https://windscribe.com/install/desktop/linux_deb_x64/beta";
  };
  unpackCmd = "${dpkg}/bin/dpkg-deb -x $curSrc .";

  nativeBuildInputs = [ autoPatchelfHook makeWrapper ];
  buildInputs = [
  ];

}
