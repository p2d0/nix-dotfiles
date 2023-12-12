{ pkgs, ... }:

pkgs.poetry2nix.mkPoetryApplication{
  projectDir = pkgs.fetchFromGitHub {
    owner = "cyfinoid";
    repo = "hbctool";
    rev = "main";
    sha256 = "sha256-+mKprmP340UkDrnVy2ppQYlNPKYizpw8wVpwrireza8=";
  };
}
  # with import <nixos-unstable> {};
  # with pkgs;
  # pkgs.python310Packages.buildPythonApplication rec {
  #   pname = "hbctool";
  #   version = "0.1.5";
  #   # src = fetchFromGitHub {
  #   #   owner = "cyfinoid";
  #   #   repo = "hbctool";
  #   #   rev = "main";
  #   #   sha256 = "sha256-+mKprmP340UkDrnVy2ppQYlNPKYizpw8wVpwrireza8=";
  #   # };
  #   src = pkgs.python310Packages.fetchPypi {
  #     inherit pname version;
  #     sha256 = "sha256-MK6wUAlbpQfaLtJuqOZKdc1j1FnQ2y7rmeGJ+4QhyKM=";
  #   };
  #   doCheck = false;
  #   buildInputs = [
  #     pkgs.python310Packages.docopt
  #   ];
  #   propagatedBuildInputs = [
  #     pkgs.python310Packages.docopt
  #   ];
  # }
