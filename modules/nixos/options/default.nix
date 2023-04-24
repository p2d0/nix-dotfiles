{ config, lib, pkgs, ... }:


# https://nixos.org/manual/nixos/stable/index.html#sec-writing-modules
#

with lib;
{
  options = {
    user = mkOption {
      type = types.str;
    };
    tests = mkOption {
      type = types.anything;
    };
  };
}
