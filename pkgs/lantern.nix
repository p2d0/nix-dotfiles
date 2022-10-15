{stdenv,pkgs, lib}:

with pkgs;
let libpcap =
      (pkgs.libpcap.overrideAttrs (old: {
        postFixup = ''
        ln -s $out/lib/libpcap.so $out/lib/libpcap.so.0.8
      '';}));
in stdenv.mkDerivation rec {

  pname = "lantern";
  version = "6.10.5";

  src = fetchurl {
    url = "https://s3.amazonaws.com/lantern/lantern-installer-${version}-64-bit.deb";
    sha256 = "sha256-6fxk6ltCYqWH6yW9XaqQl8kB1kYxp1qMyn1+Zh6t4Bs=";
  };

  unpackCmd = "${dpkg}/bin/dpkg-deb -x $curSrc .";

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];

  buildInputs = [
    libpcap
    libappindicator-gtk3
  ];

  runtimeDependencies = [
    libpcap
    libappindicator-gtk3
  ];

  # TODO Cleanup
  installPhase = ''
    mkdir -p $out
    cp -r . $out/
    rm $out/bin/lantern
    ln -s $out/lib/lantern/lantern-binary $out/bin/lantern
    chmod +x $out/lib/lantern/lantern-binary
    wrapProgram $out/lib/lantern/lantern-binary \
    --prefix PATH ":" ${lib.makeBinPath [
      libpcap
      libappindicator-gtk3
    ]}
  '';

  meta = with lib; {
    description = "VPN";
    platforms = [ "x86_64-linux" ];
    license = licenses.unfree;
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
  };
}
