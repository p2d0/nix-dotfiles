{ appimageTools, lib, fetchurl }:

# with import <nixos-unstable> {};
appimageTools.wrapType2 rec {
  pname = "alvr";
  version = "v19.1.0";

  src = fetchurl {
    url = "https://github.com/alvr-org/ALVR/releases/download/${version}/ALVR-x86_64.AppImage";
    sha256 = "sha256-cnoh2L0S6K6Rz8Kgsdhx0FcKGOPqvgSQMLgFX2n+2Fw=";
  };

  extraPkgs = pkgs: with pkgs; [ icu ];

  # extraInstallCommands =
  #   let contents = appimageTools.extract { inherit pname version src; };
  #   in ''
  #     mv -v $out/bin/${pname}-${version} $out/bin/osu\!
  #     install -m 444 -D ${contents}/osu\!.desktop -t $out/share/applications
  #     for i in 16 32 48 64 96 128 256 512 1024; do
  #       install -D ${contents}/osu\!.png $out/share/icons/hicolor/''${i}x$i/apps/osu\!.png
  #     done
  #   '';

  meta = with lib; {
    description = "ALVR";
    homepage = "ALVR";
    maintainers = [ maintainers.p720 ];
    mainProgram = "alvr";
    platforms = [ "x86_64-linux" ];
  };
}
