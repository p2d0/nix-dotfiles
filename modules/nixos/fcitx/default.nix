{ config, lib, pkgs, ... }:

{
  i18n = {
    inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5.waylandFrontend = true;
      fcitx5.addons = with pkgs; [ fcitx5-mozc fcitx5-gtk ];
    };
  };
  # TODO all
  home-manager.users.${config.user} =
    { pkgs, config, fetchFromGitHub, callPackage, ... }: {
      home.file = {
        ".config/fcitx5" = {
          source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/fcitx5;
        };
      };
    };
}
