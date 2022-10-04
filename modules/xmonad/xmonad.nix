{ config, lib, pkgs, ... }:

{
  xsession.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
  home.file = {
    ".xmonad/lib" = {
      source = ./lib;
      recursive = true;
    };

    ".xmonad/xmonad.hs" = { source = ./xmonad.hs; };
  };
}
