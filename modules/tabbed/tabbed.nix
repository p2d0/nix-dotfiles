{ config, lib, pkgs, ... }:

pkgs.tabbed.override {
  customConfig = builtins.readFile ./config.h;
}
