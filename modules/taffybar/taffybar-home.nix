{ config, lib, pkgs, ... }:

{
  home.file = {
    ".config/taffybar/taffybar.css" = {
      source = ./taffybar.css;
    };

    ".config/taffybar/gotham.css" = {
      source = ./gotham.css;
    };
  };
}
