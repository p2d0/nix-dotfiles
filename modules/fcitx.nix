{ config, lib, pkgs, ... }:

{
  i18n = {
    inputMethod = {
      enabled = "fcitx5";
      fcitx5.addons = with pkgs; [ fcitx5-mozc ];
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

  home-manager.users."${config.user}-work" =
    { pkgs, config, fetchFromGitHub, callPackage, ... }: {
      home.file = {
        ".config/fcitx5" = {
          source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/fcitx5;
        };
      };
    };
}
