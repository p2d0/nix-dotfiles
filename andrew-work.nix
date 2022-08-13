{ config, lib, pkgs, ... }:

{
  specialisation.work = {
    configuration = {
      services.xserver.displayManager.autoLogin = {
        enable = true;
        user = "${config.user}-work";
      };

      networking.extraHosts = ''
        127.0.0.1 youtube.com
        127.0.0.1 www.youtube.com
        # 127.0.0.1 reddit.com
        # 127.0.0.1 www.reddit.com
      '';

    };
    inheritParentConfig = true;
  };

  home-manager.users."${config.user}-work" =
    { pkgs, guake, fetchFromGitHub, callPackage, ... }: {
      imports = [
        ./common.nix
      ];

      home.packages = [

        # TODO localize
        (pkgs.php74.buildEnv {
          extensions = ({ enabled, all }: enabled ++ (with all; [
            xdebug
          ]));
          extraConfig = ''
      xdebug.mode=debug
    '';
        })
        pkgs.php74.packages.composer
        pkgs.cabal2nix
        pkgs.jupyter
        pkgs.docker-compose
      ];

      programs.git = {
        extraConfig = {
          safe.directory = "*";
        };
      };

      programs.fish.shellInit = ''
            function rebuild
              sudo nixos-rebuild switch
              sudo /run/current-system/specialisation/work/activate
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

      xsession = {
        initExtra = ''
              feh --bg-fill /etc/nixos/work-bg.jpg;
            '';
      };

      systemd.user.tmpfiles.rules = [
        "L /home/${config.user}-work/Downloads - - - - /mnt/md127/Downloads"
        "L /home/${config.user}-work/Documents - - - - /mnt/md127/Documents"
        "L /home/${config.user}-work/Videos - - - - /mnt/md127/Videos"
      ];
    };
}
