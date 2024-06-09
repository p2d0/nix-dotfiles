{ config, lib, pkgs, ... }:

lib.runTests {
  # testLoadModule = {
  #   expr = import ../modules/nixos/guake/default.nix {inherit pkgs lib;};
  # };
}
