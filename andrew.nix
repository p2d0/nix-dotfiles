{ config, lib, pkgs, ... }:

{
  specialisation.default = {
    configuration = {
      programs.steam.enable = true;
      hardware.opentabletdriver.enable = true;
      services.xserver.displayManager.autoLogin = {
        enable = true;
        user = config.user;
      };

    };
    inheritParentConfig = true;
  };

  home-manager.users.${config.user} =
    { pkgs, guake, fetchFromGitHub, callPackage, ... }: {
      imports = [
        ./common.nix
      ];
      home.stateVersion = "22.05";
      home.packages = [
        pkgs.obs-studio
        pkgs.lutris
        pkgs.stremio
        pkgs.chatterino2
      ];
      programs.fish.shellInit = ''
        function rebuild
          sudo nixos-rebuild switch -j4 --fast --no-flake
          sudo /run/current-system/specialisation/default/activate
        end
        function activate-specialisation
          sudo /run/current-system/specialisation/work/activate
        end'';

      qt = {
        enable = true;
        platformTheme = "gnome";
        style = {
          name = "breeze-dark";
          package = pkgs.breeze-qt5;
        };
      };

      gtk = {
        enable = true;
        theme = {
          name = "Breeze-Dark";
          package = pkgs.breeze-gtk;
        };
        iconTheme = {
          name = "Obsidian";
          package = pkgs.iconpack-obsidian;
        };
      };

      # home.file = {
      #   ".config/OpenTabletDriver" = {
      #     source = ./configs/OpenTabletDriver;
      #     recursive = true;
      #   };
      # };

      # https://nixos.wiki/wiki/Flakes

      xsession = {
        initExtra = ''
          feh --bg-fill /etc/nixos/bg_old.png;
        '';
      };

      # TODO is it a good way?
      systemd.user.tmpfiles.rules = [
        "L /home/${config.user}/Downloads - - - - /mnt/md127/Downloads"
        "L /home/${config.user}/Documents - - - - /mnt/md127/Documents"
        "L /home/${config.user}/Videos - - - - /mnt/md127/Videos"
        "L /home/${config.user}/.ssh - - - - /mnt/md127/backup_arch/.ssh"
      ];
    };
}
