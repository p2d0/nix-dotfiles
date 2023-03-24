{ pkgs ? import <nixpkgs> {} }:

with pkgs;
buildGoPackage {
  name = "psiphon";
  src = fetchFromGitHub {
    owner = "Psiphon-Labs";
    repo = "psiphon-tunnel-core";
    rev = "df9b786ba0f473c914d3db902c8b1f94a79020c2";
    sha256 = "sha256-JI63CeG7pxdUOOjWyhKSsjvkkpiNM2caqI7NeS4HtdQ=";
  } ;


  subPackages = [
    #"ClientLibrary"
    "ConsoleClient"
  ];
  tags = ["PSIPHON_DISABLE_QUIC"];
  goPackagePath = "github.com/Psiphon-Labs/psiphon-tunnel-core";
  #goDeps = ./deps.nix;
  postInstall = ''
  mv $out/bin/ConsoleClient $out/bin/psiphon
'';
  meta = with lib; {
    description = "Psiphon";
  };
}
