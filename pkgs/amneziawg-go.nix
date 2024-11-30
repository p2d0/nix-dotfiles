{ lib, buildGoModule, fetchFromGitHub, testers, wireguard-go }:

# with import <nixpkgs> {};
buildGoModule rec {
  pname = "amneziawg-go";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "amnezia-vpn";
    repo = "amneziawg-go";
    rev = "master";
    sha256 = "sha256-Xw2maGmNnx0+GO3OWS1Gu77oB9wh2dv+WobypQotUMA=";};

  postPatch = ''
    # Skip formatting tests
    rm -f format_test.go
  '';

  vendorHash = "sha256-zXd9PK3fpOx/YjCNs2auZWhbLUk2fO6tyLV5FxAH0us=";

  subPackages = [ "." ];

  ldflags = [ "-s" "-w" ];

  # postInstall = ''
  #   mv $out/bin/wireguard $out/bin/wireguard-go
  # '';

  passthru.tests.version = testers.testVersion {
    package = wireguard-go;
    version = "v${version}";
  };

  meta = with lib; {
    description = "Userspace Go implementation of WireGuard";
    homepage = "https://git.zx2c4.com/wireguard-go/about/";
    license = licenses.mit;
    maintainers = with maintainers; [ kirelagin yana zx2c4 ];
    mainProgram = "wireguard-go";
  };
}
