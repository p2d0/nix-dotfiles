{ pkgs ? import <nixpkgs> {} }:

with pkgs;
buildGoPackage {
  name = "psiphon";
  src = fetchFromGitHub {
    owner = "Psiphon-Labs";
    repo = "psiphon-tunnel-core";
    rev = "87f8e6d118bdd57942a6902fedbf814225b0092d";
    sha256 = "JIiupsNxNyvDH6zCjQ8VIilfo3LQkATixEX2Bl3XNnw=";
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
