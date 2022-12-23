{ stdenv
, lib
, fetchurl
, dpkg
, autoPatchelfHook
, makeWrapper
, dbus
, nftables
}:

stdenv.mkDerivation rec {
  pname = "cloudflare-warp";
  version = "2022.10.116";

  src = fetchurl {
    url = "https://pkg.cloudflareclient.com/uploads/cloudflare_warp_2022_10_116_1_amd64_87fd53d338.deb";
    sha256 = "sha256-L0NN5rfbp+2v9YUN3wE5pwEdlKr6YdEcNAJQX63HMDw=";
  };

  nativeBuildInputs = [
    dpkg
    autoPatchelfHook
    makeWrapper
  ];

  buildInputs = [ dbus ];

  unpackPhase = ''
    dpkg-deb -x ${src} ./
  '';
  # TODO fix systemctl service enabling
  installPhase = ''
    runHook preInstall

    mv usr $out
    mv bin $out
    mv etc $out
    mv lib/systemd/system $out/lib/systemd/
    substituteInPlace $out/lib/systemd/system/warp-svc.service \
      --replace "ExecStart=" "ExecStart=$out"
    substituteInPlace $out/lib/systemd/user/warp-taskbar.service \
      --replace "ExecStart=" "ExecStart=$out"

    runHook postInstall
  '';

  postInstall = ''
    wrapProgram $out/bin/warp-svc --prefix PATH : ${lib.makeBinPath [ nftables ]}
  '';

  meta = with lib; {
    description = "Replaces the connection between your device and the Internet with a modern, optimized, protocol";
    homepage = "https://pkg.cloudflareclient.com/packages/cloudflare-warp";
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
    license = licenses.unfree;
    maintainers = with maintainers; [ wolfangaukang ];
    platforms = [ "x86_64-linux" ];
  };
}
