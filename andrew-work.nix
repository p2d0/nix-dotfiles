{ config, lib, pkgs, ... }:

{
  specialisation.work = {
    configuration = {
      services.xserver.displayManager.autoLogin = {
        enable = true;
        user = "${config.user}-work";
      };

      networking.extraHosts = ''
        # 127.0.0.1 youtube.com
        # 127.0.0.1 www.youtube.com
        # 127.0.0.1 reddit.com
        # 127.0.0.1 www.reddit.com
        # 127.0.0.1 www.osu.ppy.sh
        # 127.0.0.1 osu.ppy.sh
      '';

    };
    inheritParentConfig = true;
  };

  home-manager.users."${config.user}-work" =
    { pkgs, guake, fetchFromGitHub, callPackage, ... }: {
      imports = [
        ./common.nix
      ];
      # home.stateVersion = "22.05";
      home.packages = [
        pkgs.python-language-server
        pkgs.postman
        pkgs.python3
        # TODO localize
    #     (pkgs.php74.buildEnv {
    #       extensions = ({ enabled, all }: enabled ++ (with all; [
    #         xdebug
    #       ]));
    #       extraConfig = ''
    #   xdebug.mode=debug
    # '';
    #     })
    #     pkgs.php74.packages.composer
      ];

      programs.git = {
        extraConfig = {
          safe.directory = "*";
        };
      };

      programs.fish.shellInit = ''
            function rebuild
              sudo nixos-rebuild switch --fast --impure --flake '/etc/nixos/.?submodules=1#mysystem'
              sudo /run/current-system/specialisation/work/activate
            end
            function activate-specialisation
              sudo /run/current-system/specialisation/work/activate
            end'';
      xdg.systemDirs.data = [
        "/etc/nixos/configs/darkman"
      ];
      services.xsettingsd = {
        enable = true;
      };
      qt = {
        enable = true;
        platformTheme = "gtk";
        # style = {
        #   name = "adwaita";
        #   package = pkgs.breeze-qt5;
        # };
      };

      # gtk = {
      #   enable = true;
      #   theme = {
      #     name = "Adwaita";
      #     package = pkgs.breeze-gtk;
      #   };
      #   iconTheme = {
      #     name = "Obsidian";
      #     package = pkgs.iconpack-obsidian;
      #   };
      # };

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
