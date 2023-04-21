{ config, lib, pkgs, ... }:

{
  imports = [
    ./modules/i3/i3.nix
    ./modules/polybar/polybar.nix
    ./modules/ssh/ssh.nix
    ./modules/fish/fish.nix
    ./modules/rofi/rofi.nix
    ./modules/gimp/gimp.nix
    ./modules/gnome-boxes.nix
  ];
  home.packages = [
    pkgs.cask
  ];

  xsession = {
    enable = true;
    profileExtra = ''
    dbus-update-activation-environment --systemd DISPLAY;'';
    #     eval $(/run/wrappers/bin/gnome-keyring-daemon --start --daemonize)
    #     export SSH_AUTH_SOCK
    #   '';
  };
  # services.flameshot.enable = true;
  #  use of undeclared identifier 'wl_proxy_marshal_flags'
  # programs.chromium =
  #   let unstable = import <nixos-unstable> {};
  #   in {
  #     enable = true;
  #     package = (unstable.ungoogled-chromium.mkDerivation (base: rec {
  #       name = "dark-mode-chromium";
  #       patches = [
  #         (pkgs.fetchpatch {
  #           url = "https://raw.githubusercontent.com/PF4Public/gentoo-overlay/84f77f55a09f6ce0523591d7c300bf9deb2a1c01/www-client/ungoogled-chromium/files/gtk-fix-prefers-color-scheme-query.diff";
  #           sha256 = "sha256-y0v+ZArCsKVKGYSBChpugvPUmXiwnAxYHkVhAgT/aOk=";
  #         })];
  #     }));
  #   };
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/plain" = "gedit.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/chrome" = "firefox.desktop";
      "text/html" = "firefox.desktop";
      "application/x-extension-htm" = "firefox.desktop";
      "application/x-extension-html" = "firefox.desktop";
      "application/x-extension-shtml" = "firefox.desktop";
      "application/xhtml+xml" = "firefox.desktop";
      "application/x-extension-xhtml" = "firefox.desktop";
      "application/x-extension-xht" = "firefox.desktop";
      "x-scheme-handler/sidequest" ="SideQuest.desktop";
    };
    # associations.added = {
    #   "x-scheme-handler/http" = "firefox.desktop";
    #   "x-scheme-handler/https" = "firefox.desktop";
    #   "x-scheme-handler/chrome" = "firefox.desktop";
    #   "text/html" = "firefox.desktop";
    #   "application/x-extension-htm" = "firefox.desktop";
    #   "application/x-extension-html" = "firefox.desktop";
    #   "application/x-extension-shtml" = "firefox.desktop";
    #   "application/xhtml+xml" = "firefox.desktop";
    #   "application/x-extension-xhtml" = "firefox.desktop";
    #   "application/x-extension-xht" = "firefox.desktop";
    # };
  };
  programs.git = {
    enable = true;
    userName = "patriot720";
    userEmail = "cerkin-3@yandex.ru";
    extraConfig = {
      core.autocrlf = false;
    };
    aliases = {
      coa = "!git add -A && git commit -m";
    };
  };
  services.xsettingsd = {
    enable = true;
  };
  modules.fish.enable = true;

  xdg.userDirs = {
    enable = true;
  };
  services.gnome-keyring.enable = true;
  services.lorri.enable = true;
  manual.json.enable = true;
  services.kdeconnect.enable = true;
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowBroken = true;
  programs.alacritty = {
    enable = true;
    settings = {
      window.opacity = 0.7;
    };
  };
  programs.direnv = {
    enable = true;
  };
  dconf.settings = {
    "org/gnome/desktop/interface" = {
      monospace-font-name = "Noto Sans Mono 10";
      font-name = "Noto Sans 10";
    };
  };

  home.stateVersion = "22.11";

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
    ".npmrc".text = "prefix = \${HOME}/.npm-packages";
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

    ".config/sway" = {
      source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/sway;
    };
    ".config/waybar" = {
      source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/waybar;
    };

    ".ideavimrc" = {
      source = ./configs/ideavim/.ideavimrc;
    };

    ".intellimacs" = {
      source = ./configs/ideavim/.intellimacs;
      recursive = true;
    };
    ".config/darkman".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/darkman;
    ".config/redshift.conf" = {
      source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/redshift.conf;
    };

    ".config/picom/picom.conf" = {
      source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/picom/picom.conf;
    };

    ".local/share/nautilus" = {
      source = ./configs/nautilus;
      recursive = true;
    };
  };
}
