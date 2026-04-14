{ appimageTools, lib, fetchurl, makeDesktopItem, pkgs }:

let
  pname = "antigravity-manager";
  version = "4.1.31";

  src = fetchurl {
    url =
      "https://github.com/lbjlaq/Antigravity-Manager/releases/download/v${version}/Antigravity.Tools_${version}_amd64.AppImage";
    sha256 = "1x3c6j3wc5psy1czqr41zp2gapn4kli7hvxkgijj7k3gkx15axd8";
  };

  appimageContents = appimageTools.extractType2 { inherit pname version src; };
in appimageTools.wrapType2 rec {
  inherit pname version src;

  extraPkgs = pkgs: with pkgs; [ ];

  desktopItem = makeDesktopItem {
    name = pname;
    exec = pname;
    icon = pname;
    desktopName = "Antigravity Manager";
    genericName = "Settings Manager";
    categories = [ "Utility" ];
  };

  extraInstallCommands = ''
    install -m 444 -D ${desktopItem}/share/applications/${pname}.desktop $out/share/applications/${pname}.desktop
    install -m 444 -D ${appimageContents}/antigravity_tools.png $out/share/icons/hicolor/512x512/apps/${pname}.png
  '';

  meta = with lib; {
    description = "Antigravity Manager";
    platforms = [ "x86_64-linux" ];
    license = licenses.unfree;
    mainProgram = pname;
  };
}
