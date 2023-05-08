{ config, lib, pkgs, ... }:
# Older versions
# https://lazamar.co.uk/nix-versions/
with lib;
let cfg = config.modules.vpn;
    oldPkgs = import (builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/c2c0373ae7abf25b7d69b2df05d3ef8014459ea3.tar.gz";
    }) {};
in {
  options.modules.vpn = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };

  config = mkIf cfg.enable (lib.my.withHome
    ({config,...}: {
      home.file = {
        ".config/qv2ray".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/nixos/vpn/qv2ray;
      };})
    {
      environment.systemPackages = with pkgs;
        [
          oldPkgs.v2ray


          # my.psiphon
          # my.lantern
        ];

    } );
}
