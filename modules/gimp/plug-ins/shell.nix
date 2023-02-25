{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {

  shellHook = ''
  export PYTHONPATH=${pkgs.gimp}/lib/gimp/2.0/python
  '';
  buildInputs = [

    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
