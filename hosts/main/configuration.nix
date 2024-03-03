# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, self, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];
  modules.flakes.enable = true;
  modules.maestral.enable = true;
  modules.gpu-screen-recorder.enable = true;

  user = self.user;

  xdg.portal = {
    enable = true;
    extraPortals = [ # pkgs.xdg-dbus-proxy pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-gnome pkgs.xdg-desktop-portal-kde
    ];
  };
  xdg.portal.config.common.default = "*";

  security.rtkit.enable = true;
  security.pki.certificateFiles = [
    (builtins.fetchurl {
      url = "https://gu-st.ru/content/lending/russian_trusted_root_ca_pem.crt";
    })
    (builtins.fetchurl {
      url = "https://gu-st.ru/content/lending/russian_trusted_sub_ca_pem.crt";
    })
  ];

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel
                                               vaapiVdpau libvdpau-va-gl
                                               amdvlk
                                               # libva1
                                               # libva
                                               rocm-opencl-icd
                                               rocm-opencl-runtime
                                               intel-ocl
                                             ];
  # rocmTargets = ["gfx803"];
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel ];
  services.ratbagd.enable = true;

  # boot.kernelPackages = pkgs.linuxPackages_zen;
  boot.loader.grub.enable = true;
  boot.loader.grub.default = 2;

  boot.blacklistedKernelModules = [ "iTCO_wdt" "iTCO_vendor_support" ];

  boot.tmp.useTmpfs = true;
  # boot.tmp.cleanOnBoot = true;

  boot.loader.grub.device = "/dev/disk/by-id/ata-BRAVEEAGLE_SSD_240GB_AA00000000540";

  networking.hostName = config.user;

  time.timeZone = "Europe/Moscow";
  boot.extraModprobeConfig = ''
    options usbhid mousepoll=1
  '';

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  services.gvfs.enable = true;
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ACTION=="add", ATTR{idVendor}=="0e8d", ATTR{idProduct}=="201d" MODE="0777" GROUP="users"
    KERNEL=="hidraw*", MODE="0666"
    KERNEL=="hiddev*", MODE="0666"
    KERNEL=="ttyUSB*", MODE="0666"
  '';

  #  programs.hyprland = {
  #    enable = true;
  #  };
  programs.droidcam.enable = true;
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
    # windowManager.i3.package = (import (builtins.fetchTarball {
    #     url = "https://github.com/NixOS/nixpkgs/archive/79b3d4bcae8c7007c9fd51c279a8a67acfa73a2a.tar.gz";
    # }) {}).i3;
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
      defaultSession = "none+i3";
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
  modules.sway.enable = false;
  modules.hjkl.enable = true;
  modules.printing3d.enable = true;
  modules.warp.enable = true;
  modules.keyrings.enable = true;

  users.users.${config.user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC2DE1tCBC6IebCR4XC7/o1yZj1r+NI7n9nhQchP6n58iJPB20u0CWdIN6EVOIOjjMzMAxAA2mKU8faJVaV5MgY6Q4F7RtbKk/gwwhH3WfDI/HYtYDkvceBlTzQ6LGnnca1A++S7tn8J8V6jp+b5ISyw7QyL0mXLk88IyxUUpcMTU4Kfl+lDUSvKnHAu4KjoRATff4b6aVjUzdVrmoJm41lxq7aMmFQaUgK+1yD89lRWSVYa49HQ7UISSpYaHnx1sdfcwJntub97lTxwOtrOfBSBX5WPXmGf1dfC6eBf5ZL1XoLoA64n7StXzrru4PJ1NHIbJSrGmAzJP7Q1zrRBjwf andrew@DESKTOP-FFBHS36"
    ];
  };
  # TODO Extract to fish module
  security.sudo = {
    enable = true;
    extraRules = [{
      commands = [
        {
          command = "/run/current-system/sw/bin/nixos-rebuild";
          options = [ "NOPASSWD" ];
        }
        {
          command = "/run/current-system/specialisation/work/activate";
          options = [ "NOPASSWD" ];
        }
        {
          command = "/run/current-system/specialisation/default/activate";
          options = [ "NOPASSWD" ];
        }];
      groups = [ "wheel" ];
    }];
  };
  programs.fish.enable = true;
  users.defaultUserShell = pkgs.fish;

  modules.emacs-with-doom.enable = true;

  # nixpkgs.config =
  #   let nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
  #   in {
  #     # allowUnfree = true;
  #     # allowBroken = true;
  #     # permittedInsecurePackages = [ "xrdp-0.9.9" "libdwarf-20181024" "python-2.7.18.6"];
  #     packageOverrides = pkgs: {
  #       # get-pr-override 218037
  #       # pr218037 = import (fetchTarball
  #       #   "${nixpkgs-tars}84963237b438319092a352a7d375878d82beb1ca.tar.gz") {
  #       #     config = config.nixpkgs.config;
  #       #   };
  #       # pr181605 = import (fetchTarball
  #       #   "${nixpkgs-tars}7cc979502c3dc5480ef3e4ffe1a05c897084d34b.tar.gz") {
  #       #     config = config.nixpkgs.config;
  #       #   };
  #       # latest-commit = import (fetchTarball
  #       #   "${nixpkgs-tars}683f25a6af6e5642cd426c69a4de1d434971a695.tar.gz") {
  #       #     config = config.nixpkgs.config;
  #       #   };
  #     };
  #   };

  services.blueman.enable = true;
  programs.dconf.enable = true;


  services = {
    #gnome.gnome-keyring.enable = true;
    dbus = {
      enable = true;
      packages = [];
    };
    # xrdp.enable = true;
    # xrdp.defaultWindowManager = "dbus-launch --exit-with-session;i3;";
    cron = {
      enable = true;
      systemCronJobs = [
        "*/30 11-21 * * * andrew DBUS_SESSION_BUS_ADDRESS='unix:path=/run/user/1000/bus' /run/current-system/sw/bin/notify-send 'Update daily'"
        # "30 20 * * * andrew fish -c 'update-system'"
        "00 23 * * * root sh -c 'shutdown now'"
        "00 22 * * * root sh -c 'shutdown now'"
        "00 21 * * * root sh -c 'shutdown now'"
        "00 20 * * * andrew fish -c 'sync_repos'"
        "3,8,13,18,23,28,33,38,43,48,53,58 * * * * andrew sleep 50 ; wget --no-check-certificate -O - https://freedns.afraid.org/dynamic/update.php?RnBTMHFiQlhHWnVmUXpNYmtLWlQ0ZXB5OjIxNjg5NzI5 >> /tmp/freedns_ug_kyrgyzstan_kg.log 2>&1 &"
      ];
    };
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

  modules.xdg.sessionVariables = true;
  # programs.java = {
  #   enable = true;
  #   package = pkgs.openjdk8;
  #   # https://javadl.oracle.com/webapps/download/GetFile/1.8.0_281-b09/89d678f2be164786b292527658ca1605/linux-i586/jdk-8u281-linux-x64.tar.gz
  #   # TODO direct link
  # };

  modules.fonts.enable = true;
  modules.timed-shutdown.enable = false;
  modules.timed-shutdown.time = "23:00:00";
  modules.darkman.enable = true;
  modules.vpn.enable = true;
  modules.vm.enable = false;
  # zramSwap.enable = true;
  # zramSwap.writebackDevice = "/dev/sdb1";
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
      (pkgs.python3.withPackages(ps: [ ps.python-lsp-server ps.python-miio ps.requests ps.epc ps.lxml ps.tld ps.sexpdata ps.pyqt6 ps.pyqt6-sip ps.pyqt6-webengine ps.pygetwindow ]))
      my.hbctool
      vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      unstable.lapce
      OSCAR
      charles
      wget
      clang
      file
      kdenlive
      openssl
      # pr218037.microsoft-edge-dev
      gcc.cc.libgcc
      conda
      megasync
      unstable.gamescope
      yandex-disk
      shared-mime-info
      clinfo
      qrencode
      # pr229886.amdgpu-pro-libs.amf
      # pr229886.amdgpu-pro-libs.vulkan
      # pr229886.amdgpu-pro-libs.opengl
      # pr229886.amdgpu-pro-libs.prefixes
      # unstable.amf-headers
      glibc
      xmrig
      sumneko-lua-language-server
      luarocks
      cloudflare-warp
      skypeforlinux
      neovide
      lazygit
      gmsh
      # unstable.microsoft-edge
      calculix
      # pr229886.amdgpu-pro-libs.amf
      # pr229886.amdgpu-pro-libs.prefixes
      # (microsoft-edge-dev.overrideAttrs(oldAttrs: rec {
      #   name = "edge-dev";
      #   version = "110.0.1587.1";
      #   src = builtins.fetchurl {
      #     url = "https://packages.microsoft.com/repos/edge/pool/main/m/microsoft-edge-dev/microsoft-edge-dev_110.0.1587.1-1_amd64.deb";
      #     sha256 = "sha256:1p39llchnb2b6zbjpn0fk7hp7yhfp03b00s539hhgaliqmq9z93g";
      #   };
      # }))
      flameshot
      unstable.flyctl
      # .overrideAttrs(oldAttrs: rec {
      #   NIX_CFLAGS_COMPILE = "-DUSE_WAYLAND_CLIPBOARD";
      #   CFLAGS = ["-DUSE_WAYLAND_CLIPBOARD"];
      #   # configureFlags = [
      #   #   "CPPFLAGS=-DUSE_WAYLAND_CLIPBOARD";
      #   # ];
      # })
      pciutils
      deluge
      usbutils
      # unstable.davinci-resolve
      # unstable.firefox
      # unstable.librewolf
      xcompmgr
      heroic
      killall
      xdo
      inotify-tools
      # (import (fetchTarball
      #   "https://github.com/aaronjanse/nix-eval-lsp/archive/master.tar.gz"))
      # (import (fetchTarball
      #   "https://github.com/nix-community/rnix-lsp/archive/master.tar.gz"))
      # my.shell_gpt
      # unstable.tgpt
      my.tgpt
      # (unstable.tgpt.overrideAttrs(oldAttrs: rec {
      #   version = "2.0.3";

      #   src = fetchFromGitHub {
      #     owner = "aandrew-me";
      #     repo = "tgpt";
      #     rev = "refs/tags/v${version}";
      #     hash = "sha256-4zm2dsYhN5itGto39p2Aq+9vF4iqqjCGwMWACuKSMs0=";
      #   };
      #   vendorHash = "";
      # }))

      my.pythonbin
      # my.tlala
      # my.chatgpt
      neovim
      # (callPackage /etc/nixos/pkgs/psiphon.nix { })
      # unstable.elementary-planner
      # (haskellPackages.callPackage /etc/nixos/modules/nixos/taffybar/build/taffybar.nix
      #   { })
      unstable.video-trimmer
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
      # nixfmt
      gimp
      mpv
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
      nix-tree
      gnome.gnome-sound-recorder
      # tigervnc
      # x11vnc
      cabal2nix
      dbeaver
      yad
      ccls
      wofi
      # jupyter
      docker-compose
      playerctl
      libusb
      solaar
      piper
      # rocketchat-desktop
      tetex
      gnumake
      btop
      calibre
      xorg.xdpyinfo
      # postman
      unstable.brave
      peco
      ffmpeg-full
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
      # shotcut
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
      # unstable.tdesktop
      # telegram-desktop_git
      telegram-desktop
      unstable.nil
      jpegoptim
      chatterino2
      filelight
      polkit_gnome
      glib
      gnome.dconf-editor
      gnome.gnome-characters
      minidlna
      gnome.gedit
      ntfs3g
      # gnome.gnome-boxes
      # rustdesk
      qbittorrent
      # tor-browser-bundle-bin
      # looking-glass-client
      # unstable.tg
      pkgs.cinnamon.nemo-with-extensions
      pkgs.cinnamon.nemo-fileroller
      pkgs.cinnamon.nemo-python
      # unstable.telegram-cli
      # (unstable.callPackage /etc/nixos/pkgs/nemo-preview.nix {})
      # my.nemo-preview
      # 41.2
      #   (import (builtins.fetchTarball {
      #     url = "https://github.com/NixOS/nixpkgs/archive/d1c3fea7ecbed758168787fe4e4a3157e52bc808.tar.gz";
      # }) {}).gnome.nautilus
      # 40.1
      #   (import (builtins.fetchTarball {
      #     url = "https://github.com/NixOS/nixpkgs/archive/23c10dbe320e6957f2607d8a22f9e0e36f56a235.tar.gz";
      # }) {}).gnome.nautilus
      # gnome.nautilus
      spice-vdagent
      inetutils
      zip
      xsettingsd
      # easyeffects
      # evolution
      nodejs
      iconpack-obsidian
      # libreoffice
      pkgs.onlyoffice-bin
      koreader
      # vlc
      gsettings-desktop-schemas
      # wineWowPackages.stable
      # whatsapp-for-linux
      libvirt
      dunst
      android-tools
      # sublime
      # drawio
      pipenv
      # my.immersed-vr
      # (import (builtins.fetchTarball {
      #   url = "https://github.com/NixOS/nixpkgs/archive/23c10dbe320e6957f2607d8a22f9e0e36f56a235.tar.gz";
      # }) {config.allowBroken = true;
      #     config.permittedInsecurePackages = [
      #       "python-2.7.18.6"
      #       "ffmpeg-3.4.8"
      #     ];
      #    }).natron
      # (import (builtins.fetchTarball {
      #   url = "https://github.com/NixOS/nixpkgs/archive/5c1ffb7a9fc96f2d64ed3523c2bdd379bdb7b471.tar.gz";
      # }) {config.allowBroken = true;
      #     config.permittedInsecurePackages = [
      #       "python-2.7.18.6"
      #       "ffmpeg-3.4.8"
      #     ];}).natron
      # unstable.natron
      # my.natron-bin
      natron
      unstable.scrcpy
      python39Packages.yt-dlp
      imagemagick
      # thunderbird
      python39Packages.pytest
      # libpulseaudio
      python39Packages.virtualenv
      python39Packages.pip
      anydesk
      feh
      gnome.eog
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
      guake
      # my.guake-latest
      # (callPackage /etc/nixos/pkgs/jetbrains-gateway.nix { })
    ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    startWhenNeeded = false;
    settings.PasswordAuthentication = false;
  };

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
