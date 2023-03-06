{pkgs ? import <nixpkgs> {} }:

# RUNNING:
# nix-instantiate --eval --strict . -A tests.config

with pkgs;
{
  tests = {
    config = callPackage ./config.nix { };
    util = callPackage ./test_util.nix { };
  };
}
