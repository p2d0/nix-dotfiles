{ config, lib, pkgs, ... }:

let
  unstable = import <nixos-unstable> { config.allowUnfree = true; };
in {
  specialisation.default = {
    configuration = {
      programs.steam.enable = true;
      hardware.opentabletdriver.enable = true;
      services.xserver.displayManager.autoLogin = {
        enable = true;
        user = config.user;
      };
      environment.systemPackages = [
      ];

    };
    inheritParentConfig = true;
  };

  home-manager.users.${config.user} =
    { pkgs, guake, fetchFromGitHub, callPackage, ... }: {
      imports = [
        ./common.nix
      ];
      # home.stateVersion = "22.05";
      home.packages = [
        pkgs.obs-studio
        pkgs.lutris
        pkgs.stremio
        # unstable.pkgs.osu-lazer-bin
        (unstable.pkgs.callPackage /etc/nixos/pkgs/osu-lazer-bin.nix { })
        # (unstable.pkgs.callPackage /etc/nixos/pkgs/games/osu-lazer/default.nix { })
        # (unstable.pkgs.osu-lazer.overrideAttrs(oldAttrs: rec {
        #   version = "2023.123.0";
        #   # dotnetFlags = [
        #   #   "--runtime linux-x64;"
        #   # ];
        #   src = pkgs.fetchFromGitHub {
        #     owner = "ppy";
        #     repo = "osu";
        #     rev = version;
        #     sha256 = "10GfAOsrLgQeYmzjhC/L57BK9BoM7ZM1pZmRK4+GD5c=";};
        # }))
        pkgs.chatterino2
      ];
      programs.fish.shellInit = ''
        function rebuild
          sudo nixos-rebuild switch --fast --impure --flake '/etc/nixos/.?submodules=1#mysystem'
          sudo /run/current-system/specialisation/default/activate
        end
        function activate-specialisation
          sudo /run/current-system/specialisation/default/activate
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
        #   name = "breeze";
        #   package = pkgs.breeze-qt5;
        # };
      };

      # gtk = {
      #   enable = true;
      #   # theme = {
      #   #   name = "Breeze";
      #   #   package = pkgs.breeze-gtk;
      #   # };
      #   # iconTheme = {
      #   #   name = "Obsidian";
      #   #   package = pkgs.iconpack-obsidian;
      #   # };
      # };

      # home.file = {
      #   ".config/OpenTabletDriver" = {
      #     source = ./configs/OpenTabletDriver;
      #     recursive = true;
      #   };
      # };

      # https://nixos.wiki/wiki/Flakes

      # xsession = {
      #   initExtra = ''
      #     feh --bg-fill /etc/nixos/bg_old.png;
      #   '';
      # };

      # TODO is it a good way?
      systemd.user.tmpfiles.rules = [
        "L /home/${config.user}/Downloads - - - - /mnt/md127/Downloads"
        "L /home/${config.user}/Documents - - - - /mnt/md127/Documents"
        "L /home/${config.user}/Videos - - - - /mnt/md127/Videos"
        "L /home/${config.user}/.ssh - - - - /mnt/md127/backup_arch/.ssh"
      ];
    };
}
