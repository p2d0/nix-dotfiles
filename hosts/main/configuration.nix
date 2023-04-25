# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, self, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];
  nix = {
    package = pkgs.nixFlakes; # or versioned attributes like nixVersions.nix_2_8
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  user = self.user;

  xdg.portal = {
    enable = true;
    extraPortals = [ # pkgs.xdg-dbus-proxy pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-gnome pkgs.xdg-desktop-portal-kde
    ];
  };

  security.rtkit.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel vaapiVdpau libvdpau-va-gl amdvlk
                                               rocm-opencl-icd
                                               rocm-opencl-runtime ];
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel ];

  # systemd.tmpfiles.rules = [
  #   "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.hip}"
  # ];

  virtualisation.spiceUSBRedirection.enable = true;

  # boot.kernelPackages = pkgs.linuxPackages_zen;
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

  #  programs.hyprland = {
  #    enable = true;
  #  };

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
        Option "PreferredMode" "2560x1080@75"'';
      }
      {
        output = "DVI-D-0";
        monitorConfig = ''Option "Position" "2560 0"'';
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
    # windowManager.i3.package = (pkgs.i3.overrideAttrs(oldAttrs: rec {
    #   patches = [
    #     (pkgs.fetchpatch {
    #       url = "https://raw.githubusercontent.com/mishurov/applets/master/i3patch/files/add_zoom_i3_4.10.4.patch";
    #       sha256 = "sha256-gOS2bRJQEGRqjkYK0IZ/oMapdUWwvFKNLEp3XBWzZC8=";
    #     })
    #   ];

    # }));
    # displayManager.gdm.enable = true;
    # desktopManager.gnome.enable = true;
    # displayManager.sddm.enable = true;
    # desktopManager.plasma5.enable = true;
    # desktopManager.plasma5.excludePackages = with pkgs.libsForQt5; [
    #   elisa
    #   gwenview
    #   okular
    #   oxygen
    #   khelpcenter
    #   konsole
    #   plasma-browser-integration
    #   print-manager
    # ];

    # windowManager.qtile.enable = true;
    displayManager = {
      defaultSession = "sway";
      autoLogin = {
        enable = true;
        user = config.user;
      };
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware.bluetooth.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # TODO Move to home manager user config?
  modules.sway.enable = true;
  modules.hjkl.enable = true;
  modules.printing3d.enable = true;
  modules.warp.enable = false;
  modules.keyrings.enable = true;

  users.users.${config.user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

  users.defaultUserShell = pkgs.fish;

  modules.emacs-with-doom.enable = true;

  nixpkgs.config =
    let nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
    in {
      allowUnfree = true;
      allowBroken = true;
      permittedInsecurePackages = [ "xrdp-0.9.9" "libdwarf-20181024"];
      packageOverrides = pkgs: {
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

  services.cron = {
    enable = true;
    systemCronJobs = [
      "00 20 * * * andrew fish -c 'sync_repos'"
    ];
  };

  services = {
    #gnome.gnome-keyring.enable = true;
    dbus = {
      enable = true;
      packages = [];
    };
    # xrdp.enable = true;
    # xrdp.defaultWindowManager = "dbus-launch --exit-with-session;i3;";
  };

  nix.settings.auto-optimise-store = true;
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 1d";

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  # services.getty.autologinUser = config.user;

  systemd.coredump.extraConfig = ''
    Storage=none
  '';

  environment.sessionVariables = {
    GTK_DATA_PREFIX = [ "${config.system.path}" ];
    XDG_CACHE_HOME  = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME   = "$HOME/.local/share";
    XDG_STATE_HOME  = "$HOME/.local/state";
  };

  programs.java = {
    enable = true;
    package = pkgs.oraclejre8;
  };

  modules.fonts.enable = true;
  modules.timed-shutdown.enable = true;
  modules.darkman.enable = true;
  modules.vpn.enable = true;
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
      file
      openssl
      pr218037.microsoft-edge-dev
      libgcc
      glibc
      sumneko-lua-language-server
      luarocks
      skypeforlinux
      neovide
      lazygit
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
      # .overrideAttrs(oldAttrs: rec {
      #   NIX_CFLAGS_COMPILE = "-DUSE_WAYLAND_CLIPBOARD";
      #   CFLAGS = ["-DUSE_WAYLAND_CLIPBOARD"];
      #   # configureFlags = [
      #   #   "CPPFLAGS=-DUSE_WAYLAND_CLIPBOARD";
      #   # ];
      # })
      pciutils
      davinci-resolve
      unstable.firefox
      unstable.librewolf
      xcompmgr
      killall
      xdo
      inotify-tools
      # (import (fetchTarball
      #   "https://github.com/aaronjanse/nix-eval-lsp/archive/master.tar.gz"))
      # (import (fetchTarball
      #   "https://github.com/nix-community/rnix-lsp/archive/master.tar.gz"))
      my.shell_gpt
      my.pythonbin
      my.tlala
      my.chatgpt
      neovim
      # (callPackage /etc/nixos/pkgs/psiphon.nix { })
      # unstable.elementary-planner
      # (haskellPackages.callPackage /etc/nixos/modules/nixos/taffybar/build/taffybar.nix
      #   { })
      kdiskmark
      tmux
      # Config https://github.com/elken/tabbed/blob/master/config.h
      # pkgs.tabbed.override {
      # customConfig = builtins.readFile ../files/tabbed-config.h;
      # };
      # (callPackage /etc/nixos/modules/tabbed/tabbed.nix { })
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
      # my.alvr


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
      # rocketchat-desktop
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
      # unstable.tdesktop
      # (unstable.qt6Packages.callPackage /etc/nixos/pkgs/tdesktop/tdesktop.nix {
      #   abseil-cpp = unstable.abseil-cpp_202111;
      # })
      unstable-small.tdesktop
      unstable.nil
      jpegoptim
      chatterino2
      filelight
      x11vnc
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
      # looking-glass-client
      unstable.tg
      gnome.nautilus
      spice-vdagent
      inetutils
      zip
      xsettingsd
      easyeffects
      # evolution
      nodejs
      iconpack-obsidian
      libreoffice
      koreader
      vlc
      gsettings-desktop-schemas
      wineWowPackages.stable
      # whatsapp-for-linux
      libvirt
      dunst
      android-tools
      # sublime
      drawio
      (unstable.python3.withPackages(ps: [ ps.requests ps.epc ps.lxml ps.tld ps.sexpdata ps.pyqt6 ps.pyqt6-sip ps.pyqt6-webengine ps.pygetwindow ]))
      unstable.python39Packages.yt-dlp
      python39Packages.pytest
      # libpulseaudio
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

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.startWhenNeeded = false;

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
