{ pkgs ? import <nixpkgs> {} }:

with pkgs;
buildGoPackage {
  name = "psiphon";
  src = fetchFromGitHub {
    owner = "Psiphon-Labs";
    repo = "psiphon-tunnel-core";
    rev = "master";
    sha256 = "SLwh+ComVcm+FiaE3I801tHqdVXEHQDah3XEKI4yCuk=";
  } ;

  subPackages = [
    #"ClientLibrary"
    "ConsoleClient"
  ];

  goPackagePath = "github.com/Psiphon-Labs/psiphon-tunnel-core";
  #goDeps = ./deps.nix;
  postInstall = ''
  mv $out/bin/ConsoleClient $out/bin/psiphon
'';
  meta = with lib; {
    description = "Psiphon";
  };
}
