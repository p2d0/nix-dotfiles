{ config, lib, pkgs, ... }:

pkgs.buildNpmPackage {
  pname = "little-coder";
  version = "1.4.1";

  src = pkgs.fetchFromGitHub {
    owner = "itayinbarr";
    repo = "little-coder";
    rev = "55a83528985ed590df82cca2c478b0f206209500";
    hash = "sha256-T/wHDTMsh2H2dWP1pI1fdP8e/zrSl/QZ2du7ixhxM+Y=";
  };

  npmDepsHash = "sha256-s1t5PVblis4T/Fv8WIiPKwSjmY9ZeE5bJbE2fdIL9jA=";

  dontNpmBuild = true;
}
