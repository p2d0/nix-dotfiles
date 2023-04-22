{ lib
, libGL
, libX11
, pkgs
, stdenv
, autoPatchelfHook
, makeWrapper
}:

stdenv.mkDerivation rec {
  pname = "tdesktop";
  version = "4.7.1";

  src = builtins.fetchurl {
    url = "https://telegram.org/dl/desktop/linux";
  };
  unpackCmd = "tar -xf $curSrc -C .";

  nativeBuildInputs = [ autoPatchelfHook makeWrapper ];
  buildInputs = [
    pkgs.xdg-utils
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp -r "./"* "$out"
    #cp $out/Telegram $out/share/telegram

    wrapProgram \
      $out/Telegram \
      --prefix PATH : ${lib.makeBinPath [
        libGL
        libX11
        pkgs.fontconfig
                                        ]}
  '';
}


  # pkgs.tdesktop.overrideAttrs (oldAttrs: rec {
  #   version = "4.2.0";
  #   src = pkgs.fetchFromGitHub {
  #     owner = "telegramdesktop";
  #     repo = "tdesktop";
  #     rev = "v${version}";
  #     fetchSubmodules = true;
  #     sha256 = "07fhm36394171w0rvay1x9x1br3z36z4dlpi57bkq23dvi331pxj";
  #   };
  # })
