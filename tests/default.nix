{pkgs ? import <nixpkgs> {} }:

# RUNNING:
# nix-instantiate --eval --strict . -A tests.config

let
  my = import ../lib/util.nix { lib =  pkgs.lib; };
in
with pkgs;
(my.mapTests /etc/nixos/tests (p: callPackage p {}))
# {
#   tests = ;
#     # tests = {
#     #   config = callPackage ./config.nix { };
#     #   util = callPackage ./test_util.nix { };
#     # };
# }
