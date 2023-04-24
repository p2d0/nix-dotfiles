{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.emacs-with-doom;
in{
  options.modules.emacs-with-doom = {
    enable = mkBoolOpt false;
    doom = {
      enable = mkBoolOpt true;
      forgeUrl = mkOpt types.str "https://github.com";
      repoUrl = mkOpt types.str "${cfg.doom.forgeUrl}/doomemacs/doomemacs";
      configRepoUrl = mkOpt types.str "${cfg.doom.forgeUrl}/p2d0/.doom.d";
    };
  };
  config = mkIf cfg.enable (lib.my.withHome
    ({config,...}: {
      home.file = {
        ".doom.d".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/nixos/editors/.doom.d; # TODO resolve relative paths
      };})
    {
      nixpkgs.overlays = [
        (import (builtins.fetchTarball {
          url =
            "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
          # sha256 = "sha256:195k5y6p2apy6bz3xm7vklsfm3h4vx2m412sinrzrjzxb3b5rgcj";
        }))
      ];
      services.emacs.install = true;
      services.emacs.enable = false;
      services.emacs.defaultEditor = true;
      # services.emacs.package = pkgs.emacsUnstable.override {
      #   withGTK3 = true;
      # };
      services.emacs.package = pkgs.emacsUnstable.overrideAttrs(oldAttrs: rec {
        src = pkgs.fetchFromGitHub {
          owner = "emacs-lsp";
          repo = "emacs";
          rev = "json-rpc";
          sha256 = "sha256-mnSG1MqUapaXyHHJRHv40cWUx1zRIwTM1O810ZJgRgc=";
        };
      });
      system.userActivationScripts = mkIf cfg.doom.enable {
        installDoomEmacs = ''
        if [ ! -d "$HOME/.emacs.d" ]; then
           ${pkgs.git} clone --depth=1 --single-branch "${cfg.doom.repoUrl}" "$HOME/.emacs.d"
           ${pkgs.git} clone "${cfg.doom.configRepoUrl}" "$HOME/.doom.d"
        fi
      '';
      };
    });

}
