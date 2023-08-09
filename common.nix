{ config, lib, pkgs, ... }:

{
  imports = lib.my.findAllModulePathsIn /etc/nixos/modules/home-manager;
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
      "text/plain" = "org.gnome.gedit.desktop";
      # "image/jpeg" = "feh.desktop";
      "image/bmp"= "org.gnome.eog.desktop";
      "image/gif"= "org.gnome.eog.desktop";
      "image/jpeg"= "org.gnome.eog.desktop";
      "image/jpg"= "org.gnome.eog.desktop";
      "image/pjpeg"= "org.gnome.eog.desktop";
      "image/png"= "org.gnome.eog.desktop";
      "image/tiff"= "org.gnome.eog.desktop";
      "image/webp"= "org.gnome.eog.desktop";
      "image/x-bmp"= "org.gnome.eog.desktop";
      "image/x-pcx"= "org.gnome.eog.desktop";
      "image/x-png"= "org.gnome.eog.desktop";
      "image/x-portable-anymap"= "org.gnome.eog.desktop";
      "image/x-portable-bitmap"= "org.gnome.eog.desktop";
      "image/x-portable-graymap"= "org.gnome.eog.desktop";
      "image/x-portable-pixmap"= "org.gnome.eog.desktop";
      "image/x-tga"= "org.gnome.eog.desktop";
      "image/x-xbitmap"= "org.gnome.eog.desktop";
      "image/heic"= "org.gnome.eog.desktop";
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
      "x-scheme-handler/discord-529050037532098580"="discord-529050037532098580.desktop";
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
  modules.shell.fish.enable = true;

  xdg.userDirs = {
    enable = true;
  };
  services.gnome-keyring.enable = true;
  services.lorri.enable = true;
  manual.json.enable = true;
  services.kdeconnect.enable = true;

  # systemd.user.services.kdeconnect = {
  #   Unit = {
  #     After = lib.mkForce [ "default.target" ];
  #     PartOf = lib.mkForce [ "default.target" ];
  #   };
  #   Service = {
  #     Environment = lib.mkForce ["QT_QPA_PLATFORM=wayland" "PATH=${config.home.profileDirectory}/bin"];
  #   };
  # };
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
    media_dir=/mnt/md127/Downloads/stuff
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
    ".config/gammastep.conf" = {
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
