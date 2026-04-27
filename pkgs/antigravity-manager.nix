{ appimageTools, lib, fetchurl, makeDesktopItem, pkgs }:

let
  pname = "antigravity-manager";
  version = "4.1.29";

  src = fetchurl {
    url =
      "https://github.com/lbjlaq/Antigravity-Manager/releases/download/v${version}/Antigravity.Tools_${version}_amd64.AppImage";
    sha256 = "sha256-ANOO+aM8+Oq5//3Artw5LeGjDbmP/mdsoTFp56WVtAQ=";
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
