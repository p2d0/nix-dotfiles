{ appimageTools, lib, fetchurl, pkgs }:

appimageTools.wrapType2 rec {
  pname = "antigravity-manager";
  version = "4.1.31";

  src = fetchurl {
    url = "https://github.com/lbjlaq/Antigravity-Manager/releases/download/v${version}/Antigravity.Tools_${version}_amd64.AppImage";
    sha256 = "1x3c6j3wc5psy1czqr41zp2gapn4kli7hvxkgijj7k3gkx15axd8";
  };

  extraPkgs = pkgs: with pkgs; [ ];

  meta = with lib; {
    description = "Antigravity Manager";
    platforms = [ "x86_64-linux" ];
    license = licenses.unfree;
    mainProgram = pname;
  };
}
