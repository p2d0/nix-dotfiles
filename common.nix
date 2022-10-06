{ config, lib, pkgs, ... }:

{
  imports = [
    ./modules/xmonad/xmonad.nix
    ./modules/ssh/ssh.nix
    ./modules/fish/fish.nix
    ./modules/taffybar/taffybar-home.nix
    ./modules/rofi/rofi.nix
  ];
  # home.packages = [
  # ];

  xsession = {
    enable = true;
    # profileExtra = ''
    #     eval $(/run/wrappers/bin/gnome-keyring-daemon --start --daemonize)
    #     export SSH_AUTH_SOCK
    #   '';
  };
  # services.flameshot.enable = true;
  programs.git = {
    enable = true;
    userName = "patriot720";
    userEmail = "cerkin-3@yandex.ru";
  };

  xdg.userDirs = {
    enable = true;
  };
  services.emacs.enable = true;
  services.emacs.package = pkgs.emacsNativeComp;
  services.gnome-keyring.enable = true;
  services.emacs.defaultEditor = true;
  services.lorri.enable = true;
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
  #services.emacs.package = pkgs.emacsUnstable;
  manual.json.enable = true;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  programs.direnv = {
    enable = true;
  };
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Noto Sans Mono 10";
      font-name = "Noto Sans 10";
    };
  };


  services.blueman-applet.enable = true;
  services.dropbox.enable = true;
  home.keyboard = null;
  # {layout = "us,ru"; options = [ "grp:alt_shift_toggle" ];};
  home.file = {
    # ".config/GIMP" = {
    #   source = ./configs/GIMP;
    #   recursive = true;
    # };

    # ".config/Trolltech.conf" = {
    #   source = ./configs/Trolltech.conf;
    # };
    # ".config/brave-flags.conf" = {
    #   source = ./configs/brave-flags.conf;
    # };

    ".config/dunst" = {
      source = ./configs/dunst;
      recursive = true;
    };

    ".config/minidlna.conf".text = ''
    network_interface=enp3s0
    friendly_name=Pepega Server
    inotify=yes
    media_dir=/mnt/md127/Downloads/Brooklyn.Nine-Nine.S03.Season.3.Complete.1080p.WEB-DL.X264.10Bit-[maximersk]
    '';

    ".config/albert" = {
      source = ./configs/albert;
      recursive = true;
    };

    ".config/fcitx5" = {
      source = ./configs/fcitx5;
      recursive = true;
    };

    # ".config/omf" = {
    #   source = ./configs/omf;
    #   recursive = true;
    # };

    ".config/psiphon" = {
      source = ./configs/psiphon;
      recursive = true;
    };

    ".config/qt5ct" = {
      source = ./configs/qt5ct;
      recursive = true;
    };

    ".config/slop" = {
      source = ./configs/slop;
      recursive = true;
    };

    ".config/swappy" = {
      source = ./configs/swappy;
      recursive = true;
    };

    ".config/yay" = {
      source = ./configs/yay;
      recursive = true;
    };

    ".ideavimrc" = {
      source = ./configs/ideavim/.ideavimrc;
    };

    ".intellimacs" = {
      source = ./configs/ideavim/.intellimacs;
      recursive = true;
    };

    ".config/redshift.conf" = {
      source = ./configs/redshift.conf;
    };

    ".config/picom/picom.conf" = {
      source = ./configs/picom/picom.conf;
    };

    ".local/share/nautilus" = {
      source = ./configs/nautilus;
      recursive = true;
    };
  };
}
