{ config, mkDerivation, lib, pkgs, ... }:


mkDerivation rec {
  name = "my-taffybar";
  paths = [pkgs.my-taffybar];
  buildInputs = [(pkgs.haskellPackages.ghcWithPackages
    (self: [ self.xmonad self.xmonad-contrib self.xmonad-extras ]))];
}
