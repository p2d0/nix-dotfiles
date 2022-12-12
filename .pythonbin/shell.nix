{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    pkgs.gobject-introspection
    pkgs.vte
    pkgs.python-language-server
    (pkgs.python2.withPackages(ps: [ ps.pygtk ps.requests ]))
    pkgs.gtk2
    pkgs.gtk2-x11
    pkgs.pipenv
    pkgs.gdb
    # keep this line if you use bash
    pkgs.bashInteractive
  ];
}
