{ config, lib, pkgs, ... }:

{
  home.packages = [
    pkgs.nixfmt
    pkgs.gimp
    pkgs.pulseaudio
    pkgs.btop
    pkgs.brave
    pkgs.peco
    pkgs.xdotool
    pkgs.libsForQt5.breeze-gtk
    pkgs.libsForQt5.breeze-qt5
    pkgs.pasystray
    pkgs.pavucontrol
    pkgs.paprefs
    (pkgs.callPackage ./pkgs/picom-animations.nix { })
    (pkgs.callPackage ./pkgs/guake-latest.nix { })
    (pkgs.callPackage ./pkgs/lantern.nix { })
    pkgs.speedcrunch
    pkgs.chatterino2
    pkgs.filelight
    pkgs.haskellPackages.status-notifier-item
    pkgs.gnome.dconf-editor
    pkgs.gnome.gnome-characters
    pkgs.redshift
    pkgs.flameshot
    pkgs.gnome.nautilus
    pkgs.dunst
    pkgs.tdesktop
    pkgs.feh
    pkgs.rofi
    pkgs.alacritty
    pkgs.dmenu
    pkgs.gnome.gnome-disk-utility
    pkgs.cabal2nix
    pkgs.htop
  ];

  systemd.user.services.gtk-sni-tray = {
    Unit.Description = "Gtk sni tray";
    Install.WantedBy = [ "default.target" ];
    Service = {
      ExecStart = "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
    };
  };
  xsession = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
  };
  programs.git = {
    enable = true;
    userName = "patriot720";
    userEmail = "cerkin-3@yandex.ru";
  };

  xdg.userDirs = {
    enable = true;
  };
  services.emacs.enable = true;
  services.lorri.enable = true;
  nixpkgs.overlays = [
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];
  services.emacs.package = pkgs.emacsUnstable;
  manual.json.enable = true;
  programs.fish.shellInit = ''
        fish_vi_key_bindings
        fish_add_path  $HOME/.emacs.d/bin
        fish_add_path  $HOME/.pythonbin
        function fish_user_key_bindings
            bind -M normal -m insert \cr 'peco_select_history (commandline -b)'
            bind -M insert \cr 'peco_select_history (commandline -b)'
        end'';
  programs.fish.enable = true;
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

  programs.fish.plugins = [
    {
      name = "z";
      src = pkgs.fetchFromGitHub {
        owner = "jethrokuan";
        repo = "z";
        rev = "ddeb28a7b6a1f0ec6dae40c636e5ca4908ad160a";
        sha256 = "0c5i7sdrsp0q3vbziqzdyqn4fmp235ax4mn4zslrswvn8g3fvdyh";
      };
    }
    {
      name = "peco";
      src = pkgs.fetchFromGitHub {
        owner = "oh-my-fish";
        repo = "plugin-peco";
        rev = "master";
        sha256 = "sha256-EUoicPd+aUMlfCeo9BOuIiBlQSpPtMtMn5AUkZU3uQA=";
      };
    }
  ];

  services.blueman-applet.enable = true;
  services.dropbox.enable = true;
  home.file = {
    ".xmonad/lib" = {
      source = ./dotfiles/.xmonad/lib;
      recursive = true;
    };

    ".xmonad/xmonad.hs" = { source = ./dotfiles/.xmonad/xmonad.hs; };

    ".config/rofi" = {
      source = ./dotfiles/.config/rofi;
      recursive = true;
    };
    # ".config/GIMP" = {
    #   source = ./dotfiles/.config/GIMP;
    #   recursive = true;
    # };

    # ".config/Trolltech.conf" = {
    #   source = ./dotfiles/.config/Trolltech.conf;
    # };

    ".ssh" = {
      source = /mnt/md127/backup_arch/.ssh;
      recursive = true;
    };
    ".config/dunst" = {
      source = ./dotfiles/.config/dunst;
      recursive = true;
    };
    ".config/albert" = {
      source = ./dotfiles/.config/albert;
      recursive = true;
    };
    ".config/fcitx5" = {
      source = ./dotfiles/.config/fcitx5;
      recursive = true;
    };

    ".config/omf" = {
      source = ./dotfiles/.config/omf;
      recursive = true;
    };

    ".config/psiphon" = {
      source = ./dotfiles/.config/psiphon;
      recursive = true;
    };

    ".config/qt5ct" = {
      source = ./dotfiles/.config/qt5ct;
      recursive = true;
    };

    ".config/slop" = {
      source = ./dotfiles/.config/slop;
      recursive = true;
    };
    ".config/swappy" = {
      source = ./dotfiles/.config/swappy;
      recursive = true;
    };
    ".config/yay" = {
      source = ./dotfiles/.config/yay;
      recursive = true;
    };
    # ".config/Trolltech.conf" = {
    #   source = ./dotfiles/.config/Trolltech.conf;
    # };
    ".config/redshift.conf" = {
      source = ./dotfiles/.config/redshift.conf;
    };
    ".config/picom/picom.conf" = {
      source = ./dotfiles/.config/picom/picom.conf;
    };
    ".local/share/nautilus" = {
      source = ./dotfiles/.local/share/nautilus;
      recursive = true;
    };

    ".config/taffybar/taffybar.css" = {
      source = ./taffybar/taffybar.css;
    };
    ".config/taffybar/gotham.css" = {
      source = ./taffybar/gotham.css;
    };

    ".config/fish/functions" = {
      source = ./dotfiles/.config/fish/functions;
      recursive = true;
    };
    ".config/fish/completions" = {
      source = ./dotfiles/.config/fish/completions;
      recursive = true;
    };

  };
}
