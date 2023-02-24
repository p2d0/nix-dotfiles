{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    (pkgs.python3.withPackages(ps: [ ps.gimpfu ]))

  ];
}
