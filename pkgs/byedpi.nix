{ config, lib, pkgs, ... }:

pkgs.stdenv.mkDerivation rec {
  pname = "byedpi";
  version = "0.13.1";

  src = pkgs.fetchFromGitHub {
    owner = "hufrea";
    repo = "byedpi";
    rev = "v${version}";
    sha256 = "sha256-Sf5Ik8+9nKhum4/faGf44Y/gQggldyRsFUVmd9XPKOA=";
  };

  buildInputs = [ pkgs.gnumake pkgs.gcc ];

  buildPhase = ''
          make
        '';

  installPhase = ''
          mkdir -p $out/bin
          cp ciadpi $out/bin/
        '';

  meta = with pkgs.lib; {
    description = "A simple tool to say goodbye to DPI";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
