# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, ... }:

let
  unstable = import <nixos-unstable> { config.allowBroken = true; config.allowUnfree = true; }; # https://nixos.wiki/wiki/FAQ#How_can_I_install_a_package_from_unstable_while_remaining_on_the_stable_channel.3F
  darkman = (pkgs.callPackage /etc/nixos/pkgs/darkman.nix { });
  warp = (pkgs.callPackage /etc/nixos/pkgs/warp.nix { });
in {
  imports = [
    ./hardware-configuration.nix
  ];
  nix = {
    package = pkgs.nixFlakes; # or versioned attributes like nixVersions.nix_2_8
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  user = "andrew";
  systemd.user.services.gnomo-polkit = {
    description = "Gnome polkit gui";
    serviceConfig = {
      ExecStart =
        "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
    };
    wantedBy = [ "multiuser.target" ];
    enable = true;
  };


  programs.gamemode = {
    enable = false;
    settings = {
      general = {
        renice = 10;
      };

      # Warning: GPU optimisations have the potential to damage hardware
      gpu = {
        apply_gpu_optimisations = "accept-responsibility";
        gpu_device = 0;
        amd_performance_level = "high";
      };
    };
  };

  systemd.services.warp-svc = {
    enable = true;
    description = "Warp service";
    wantedBy = [ "default.target" ];
    serviceConfig = {
      ExecStart = "${warp}/bin/warp-svc";
    };
  };

  programs.corectrl ={
    enable = false;
    gpuOverclock.enable = true;
  };


  xdg.portal = {
    enable = true;
    extraPortals = [ darkman ];
  };

  security.rtkit.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel vaapiVdpau libvdpau-va-gl ];
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel ];

  virtualisation.spiceUSBRedirection.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.loader.grub.enable = true;
  boot.loader.grub.default = 2;
  boot.loader.grub.version = 2;

  boot.blacklistedKernelModules = [ "iTCO_wdt" "iTCO_vendor_support" ];

  boot.tmpOnTmpfs = true;
  # boot.cleanTmpDir = true;

  boot.loader.grub.device = "/dev/sda";

  networking.hostName = config.user;

  time.timeZone = "Europe/Moscow";

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  services.gvfs.enable = true;
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ACTION=="add", ATTR{idVendor}=="0e8d", ATTR{idProduct}=="201d" MODE="0777" GROUP="users"
  '';

  # services.xserver.desktopManager.plasma5.excludePackages = with pkgs.libsForQt5; [
  #   elisa
  #   gwenview
  #   okular
  #   oxygen
  #   khelpcenter
  #   konsole
  #   plasma-browser-integration
  #   print-manager
  # ];
  # programs.hyprland = {
  #   enable = true;
  # };
  # programs.sway = {
  #   enable = true;
  #   wrapperFeatures.gtk = true;
  # };


  services.xserver = {
    enable = true;
    videoDrivers = [ "amdgpu" ];

    xrandrHeads = [
      {
        output = "DisplayPort-0";
        primary = true;
        monitorConfig = ''
Modeline "2560x1080@75"  228.25  2560 2608 2640 2720  1080 1083 1093 1119 +hsync -vsync
Option "PreferredMode" "2560x1080@75"

'';
      }
      {
        output = "DVI-D-0";
        monitorConfig = ''
Option "Position" "2560 0"
'';
      }
    ];
    # Doesnt work
    layout = "us,ru";
    xkbOptions = "grp:alt_shift_toggle";
    #  deviceSection = ''
    #      Option          "TearFree" "true"
    # '';
    libinput = {
      enable = true;
      mouse = { accelProfile = "flat"; };
    };
    exportConfiguration = true;
    windowManager.i3.enable = true;
    # windowManager.qtile.enable = true;
    displayManager = { defaultSession = "none+i3"; };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  sound.enable = true;
  hardware.bluetooth.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # TODO Move to home manager user config?
  modules.hjkl.enable = true;

  users.defaultUserShell = pkgs.fish;

  users.groups.gamemode = {};
  users.users.${config.user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "gamemode" "corectrl" ]; # Enable ‘sudo’ for the user.
  };

  users.users."${config.user}-work" = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

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

  nixpkgs.config =
    let nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
    in {
      allowUnfree = true;
      allowBroken = true;
      permittedInsecurePackages = [ "xrdp-0.9.9" "libdwarf-20181024"];
      packageOverrides = pkgs: {
        qtile = (pkgs.callPackage /etc/nixos/pkgs/qtile.nix {});
        # get-pr-override 218037
        # pr218037 = import (fetchTarball
        #   "${nixpkgs-tars}84963237b438319092a352a7d375878d82beb1ca.tar.gz") {
        #     config = config.nixpkgs.config;
        #   };
        # pr181605 = import (fetchTarball
        #   "${nixpkgs-tars}7cc979502c3dc5480ef3e4ffe1a05c897084d34b.tar.gz") {
        #     config = config.nixpkgs.config;
        #   };
        # latest-commit = import (fetchTarball
        #   "${nixpkgs-tars}683f25a6af6e5642cd426c69a4de1d434971a695.tar.gz") {
        #     config = config.nixpkgs.config;
        #   };
      };
    };

  services.blueman.enable = true;
  programs.dconf.enable = true;

  systemd.services.shutdown = {
    description = "Shutdown service";
    serviceConfig.Type = "oneshot";
    script = "shutdown now";
  };

  systemd.timers.shutdown = {
    description = "Shutdown timer";
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "*-*-* 22:00:00";
    timerConfig.Unit = "shutdown.service";
  };

  services.cron = {
    enable = true;
    systemCronJobs = [
      "00 20 * * * andrew fish -c 'sync_repos'"
    ];
  };

  programs.seahorse.enable = true;
  services = {
    #gnome.gnome-keyring.enable = true;
    gnome.at-spi2-core.enable = true;
    dbus = {
      enable = true;
      packages = [darkman];
    };
    # xrdp.enable = true;
    # xrdp.defaultWindowManager = "dbus-launch --exit-with-session;i3;";
  };

  nix.settings.auto-optimise-store = true;
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 2d";

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  environment.sessionVariables = {
    GTK_DATA_PREFIX = [ "${config.system.path}" ];
  };
  programs.java = { enable = true; package = pkgs.oraclejre8; };
  modules.fonts.enable = true;
  zramSwap.enable = true;
  # services.journald.extraConfig = ''
  #   SystemMaxUse=1G
  # '';

  #   environment.etc = {
  #     "docker/daemon.json" = {
  #       text = ''
  # {
  #   "data-root": "/mnt/md127/docker"
  # }
  # '';
  #     };
  #   };
  modules.taffybar.enable = false;
  environment.systemPackages = with pkgs;
    (if config.programs.hyprland.enable
     then [
       gammastep
       waybar
     ]
     else [
       redshift
     ])
    ++ [
      vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      wget
      clang
      openssl
      pr218037.microsoft-edge-dev

      # microsoft-edge
      # (microsoft-edge-dev.overrideAttrs(oldAttrs: rec {
      #   name = "edge-dev";
      #   version = "110.0.1587.1";
      #   src = builtins.fetchurl {
      #     url = "https://packages.microsoft.com/repos/edge/pool/main/m/microsoft-edge-dev/microsoft-edge-dev_110.0.1587.1-1_amd64.deb";
      #     sha256 = "sha256:1p39llchnb2b6zbjpn0fk7hp7yhfp03b00s539hhgaliqmq9z93g";
      #   };

      # }))

      flameshot
      unstable.firefox
      xcompmgr
      killall
      # (import (fetchTarball
      #   "https://github.com/aaronjanse/nix-eval-lsp/archive/master.tar.gz"))
      # (import (fetchTarball
      #   "https://github.com/nix-community/rnix-lsp/archive/master.tar.gz"))
      my.shell_gpt
      my.lantern
      my.pythonbin
      my.tlala
      my.chatgpt
      protonvpn-gui
      shadowsocks-rust
      my.psiphon
      # (callPackage /etc/nixos/pkgs/psiphon.nix { })
      warp
      # unstable.elementary-planner
      #cloudflare-warp
      (haskellPackages.callPackage /etc/nixos/modules/taffybar/build/taffybar.nix
        { })
      tmux
      # Config https://github.com/elken/tabbed/blob/master/config.h
      # pkgs.tabbed.override {
      # customConfig = builtins.readFile ../files/tabbed-config.h;
      # };
      (callPackage /etc/nixos/modules/tabbed/tabbed.nix { })
      git
      ripgrep
      wmctrl
      fd
      breeze-gtk
      breeze-qt5
      nixfmt
      gimp
      mpv
      libva
      libva-utils
      inkscape
      evince
      sidequest
      xorg.xwininfo
      xboxdrv
      mangohud
      lua
      apktool
      apksigner
      my.alvr


      jq
      pulseaudio
      gnome.gnome-system-monitor
      gnome.zenity
      gnome.gnome-sound-recorder
      tigervnc
      cabal2nix
      dbeaver
      yad
      ccls
      wofi
      jupyter
      docker-compose
      playerctl
      libusb
      rocketchat-desktop
      tetex
      gnumake
      btop
      calibre
      python-language-server
      xorg.xdpyinfo
      postman
      brave
      peco
      ffmpeg
      dfeet
      slop
      libnotify
      xclip
      xdotool
      tldr
      cheat
      libsForQt5.breeze-gtk
      libsForQt5.breeze-qt5
      pasystray
      pavucontrol
      paprefs
      shotcut
      darktable
      unstable.jetbrains.idea-community
      picom
      # (callPackage /etc/nixos/pkgs/picom-animations.nix { })
      my.puush-linux
      # (pkgs.callPackage /mnt/md127/nixpkgs/pkgs/applications/networking/instant-messengers/telegram/tdesktop { })
      # (pkgs.qt6Packages.callPackage /mnt/md127/nixpkgs/pkgs/applications/networking/instant-messengers/telegram/tdesktop {
      # abseil-cpp = pkgs.abseil-cpp_202111;
      # })
      #(pkgs.callPackage /etc/nixos/pkgs/tdesktop.nix { })
      # (pkgs.callPackage ./pkgs/openhab.nix { })
      # (callPackage ./pkgs/psiphon.nix { })
      speedcrunch
      discord
      unstable.tdesktop
      unstable.nil
      jpegoptim
      chatterino2
      filelight
      x11vnc
      haskellPackages.status-notifier-item
      polkit_gnome
      glib
      gnome.dconf-editor
      gnome.gnome-characters
      minidlna
      gnome.gedit
      ntfs3g
      gnome.gnome-boxes
      rustdesk
      qbittorrent
      tor-browser-bundle-bin
      looking-glass-client
      unstable.tg
      gnome.nautilus
      darkman
      spice-vdagent
      inetutils
      zip
      xsettingsd
      easyeffects
      evolution
      nodejs
      iconpack-obsidian
      libreoffice
      koreader
      vlc
      gsettings-desktop-schemas
      wineWowPackages.stable
      whatsapp-for-linux
      libvirt
      dunst
      android-tools
      sublime
      drawio
      (unstable.python3.withPackages(ps: [ ps.requests ps.epc ps.lxml ps.tld ps.sexpdata ps.pyqt6 ps.pyqt6-sip ps.pyqt6-webengine ps.pygetwindow ]))
      unstable.python39Packages.yt-dlp
      python39Packages.pytest
      libpulseaudio
      python39Packages.virtualenv
      python39Packages.pip
      anydesk
      feh
      alacritty
      dmenu
      gnome.gnome-disk-utility
      cabal2nix
      htop
      unzip
      audacity
      my.get_current_screen_geometry
      # (pkgs.callPackage /etc/nixos/pkgs/get_current_screen_geometry.nix { })
      # NOTE https://nixos.wiki/wiki/Nixpkgs/Modifying_Packages
      # guake
      my.guake-latest
      # (callPackage /etc/nixos/pkgs/jetbrains-gateway.nix { })
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
