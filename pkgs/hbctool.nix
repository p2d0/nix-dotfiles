{ pkgs, ... }:

# with import <nixos-unstable> {};
# with pkgs;
pkgs.python310Packages.buildPythonApplication rec {
  pname = "hbctool";
  version = "0.1.5";
  src = pkgs.python310Packages.fetchPypi {
    inherit pname version;
    sha256 = "sha256-MK6wUAlbpQfaLtJuqOZKdc1j1FnQ2y7rmeGJ+4QhyKM=";
  };
  doCheck = false;
  buildInputs = [
    pkgs.python310Packages.docopt
  ];
  propagatedBuildInputs = [
    pkgs.python310Packages.docopt
  ];
}
