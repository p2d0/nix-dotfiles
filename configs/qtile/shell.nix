{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    (pkgs.python3.withPackages(ps: [ ps.pygtk ps.requests ]))

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
