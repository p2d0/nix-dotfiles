{ config, lib, pkgs, ... }:

{
  specialisation.default = {
    configuration = {
      programs.steam.enable = true;
      programs.steam.package = pkgs.unstable.steam;
      hardware.opentabletdriver.enable = true;
      services.xserver.displayManager.autoLogin = {
        enable = true;
        user = config.user;
      };
      environment.systemPackages = [
        # (unstable.obs-studio.overrideAttrs(oldAttrs: rec {
        #   patches = [
        #     (pkgs.fetchpatch {
        #       url = "https://pastebin.com/raw/aPDNZWJK";
        #       sha256 = "sha256-20SfLfUuo8h5sKyWQAAhc1X8qgF35+8zMRjE3+o8oHU=";
        #     })];
        # }))
        (pkgs.wrapOBS {
          plugins = with pkgs.obs-studio-plugins; [
            obs-gstreamer
            obs-vkcapture
            obs-vaapi
            droidcam-obs
          ];
        })


        # pkgs.obs-studio
        (pkgs.lutris.override {
          extraPkgs = pkgs: with pkgs;[
            pkgs.alsa-lib
            # pkgs.pkgsi686Linux.alsa-lib
            pkgs.SDL2
            pkgs.libpulseaudio
            pkgs.pkgsi686Linux.libpulseaudio
            pkgs.winetricks
            pkgs.gtk3-x11
            pkgs.pango
            pkgs.gdk-pixbuf
            pkgs.shared-mime-info
            pkgs.libxcrypt
          ];
        })
        # pkgs.unstable.stremio
        pkgs.my.osu-lazer-bin
        # pkgs.unstable.osu-lazer-bin
        pkgs.chatterino2
      ];
    };
    inheritParentConfig = true;
  };

  specialisation.work = {
    configuration = {

      networking.extraHosts = ''
        # 127.0.0.1 youtube.com
        # 127.0.0.1 www.youtube.com
        # 127.0.0.1 reddit.com
        # 127.0.0.1 www.reddit.com
        # 127.0.0.1 www.osu.ppy.sh
        # 127.0.0.1 osu.ppy.sh
      '';
      environment.systemPackages = [
      ];
      environment.variables = {
        WORK_MODE = "1";
        ROC_ENABLE_PRE_VEGA = "1";
      };
      # environment.sessionVariables = {
      #   WORK_MODE = "1";
      # };
    };
    inheritParentConfig = true;
  };

  home-manager.useGlobalPkgs = true;
  home-manager.backupFileExtension = "backup";
  # xdg.configFile."mimeapps.list".force = true;

  home-manager.users.${config.user} =
    { pkgs, guake, fetchFromGitHub, callPackage, ... }: {
      imports = [
        ./common.nix
      ];
      home.packages = [
      ];

      programs.fish.shellInit = ''
        function rebuild-work
          set -x NIX_BUILD_CORES 10
          sudo nixos-rebuild switch --impure  --flake '/etc/nixos/.?submodules=1#mysystem' -j10 $argv
          sudo /run/current-system/specialisation/work/activate
        end
        function activate-specialisation-work
          sudo /run/current-system/specialisation/work/activate
        end
        function rebuild-default
          set -x NIX_BUILD_CORES 10
          sudo nixos-rebuild switch --impure  --flake '/etc/nixos/.?submodules=1#mysystem' -j10 $argv
          sudo /run/current-system/specialisation/default/activate
        end
        function update-system
          nix flake update /etc/nixos
          set -x NIX_BUILD_CORES 10
          sudo nixos-rebuild boot --impure  --flake '/etc/nixos/.?submodules=1#mysystem' -j10 $argv
        end
        function activate-specialisation-default
          sudo /run/current-system/specialisation/default/activate
        end'';

      programs = {
        direnv = {
          enable = true;
          nix-direnv.enable = true;
        };
      };

      qt = {
        enable = true;
        # TODO Doesnt work with telegram?
        # platformTheme = "gtk";
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
