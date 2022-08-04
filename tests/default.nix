{pkgs ? import <nixpkgs> {} }:

with pkgs;
{
  tests = {
    config = callPackage ./config.nix { };
  };
}
