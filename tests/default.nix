{pkgs ? import <nixpkgs> {} }:

# RUNNING:
# nix-instantiate --eval --strict . -A tests.config

with pkgs;
{
  tests = {
    config = callPackage ./config.nix { };
    multiple_users = callPackage ./test_multiple_users.nix { };
  };
}
