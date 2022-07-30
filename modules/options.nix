{ config, lib, pkgs, ... }:

with lib;
{
  options = {
    user = mkOption {
      type = types.str;
    };
  };
}
