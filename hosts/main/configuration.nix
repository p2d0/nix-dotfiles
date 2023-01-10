# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, ... }:

let
  unstable = import <nixos-unstable> { }; # https://nixos.wiki/wiki/FAQ#How_can_I_install_a_package_from_unstable_while_remaining_on_the_stable_channel.3F
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
    wantedBy = [ "default.target" ];
    enable = true;
  };

  systemd.user.services.gtk-sni-tray = {
    description = "Gtk sni tray";
    wantedBy = [ "default.target" ];
    serviceConfig = {
      ExecStart =
        "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
    };
  };
  programs.gamemode = {
    enable = true;
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
    enable = true;
    gpuOverclock.enable = true;
  };


  xdg.portal = {
    enable = true;
    extraPortals = [ darkman ];
  };

  security.rtkit.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  # hardware.opengl.extraPackages = [ pkgs.amdvlk ];

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
  services.xserver = {
    enable = true;
    videoDrivers = [ "amdgpu" ];

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
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager = { defaultSession = "none+xmonad"; };
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
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;

  nixpkgs.config =
    let nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
    in {
      allowUnfree = true;
      allowBroken = true;
      permittedInsecurePackages = [ "libdwarf-20181024" ];
      packageOverrides = pkgs:
        {
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
  services.postgresql = {
    enable = false;
    authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
    initialScript = pkgs.writeText "backend-initScript" ''
      CREATE USER jiradbadmin WITH PASSWORD 'password';
      CREATE DATABASE jiradb WITH ENCODING 'UNICODE' LC_COLLATE 'C' LC_CTYPE 'C' TEMPLATE template0;
      GRANT ALL PRIVILEGES ON DATABASE jiradb TO jiradbadmin;
    '';  };
  services.jira = {
    enable = false;
    jrePackage = pkgs.openjdk8;
    catalinaOptions = ["-javaagent:/mnt/md127/Downloads/atlassian-agent/target/atlassian-agent-jar-with-dependencies.jar"];
    package = pkgs.atlassian-jira.overrideAttrs(oldAttrs: rec {
      postInstall = ''
      rm -f $out/atlassian-jira/WEB-INF/lib/atlassian-extras-3.4.6.jar
      cd $out/atlassian-jira/WEB-INF/lib
      ${pkgs.wget} https://omwtfyb.ir/atlassian-extras-3.4.6.jar'';
    });
    # KEY: AAAB+g0ODAoPeJyNU12PojAUfedXkOzjBmzRwY+kySriyIo6DrCT9a3iVTrD17bFGffXLwhmPjRmE15o7zn3nHNvv3lFqg5zrmKkImOA+wNkqoFvqQYyDGXPAdIoy3PgustCSAX4xxwWNAFiLedz+9Fyhq5icaCSZemYSiAVUEMdDSPlBmQMIuQsr1AkSGOWMAlbNa4B6uaoRlLmYtBq/Y1YDDrLlDllqYSUpiHYbznjx6Zbr6+hbvkpz4zTs0p7y2rqhevMHd8eK4si2QBf7gIBXBANn8Xd4Mp5ti1CqVc/msh28pVy0C+IbtTSULIDEMkL+JTlx/Mb8FIVtaB0zevSJp5fZePKnKF4xeY9xlOJfaBxcRoG2dFYNPRfiZZ8T1Mm6roq6TJo3G/ruIN1bJg6xr1BDyGsWFkqS7F2GX5MhJ7oz/RAt6y8+rFPyjM9zJK6xUUsjdgpFRGZW8iarKxJ27v/zhLjPnl9Sqcrx/Lcdb4wZv31ygmmeBYf//xOWiNv7XaXuwhPneDhpZV1Ealb/GdqnqS8clr7b8bsjInrjD17obnY7PTv7rq4Y5oIf9qaa4vqAT8AL+Gj0ZOtIfcn1oKlMdEc358pL3A8DwObCHVRr93G117N5T4+FDyMqICvb+Yj+DSxnDPRmC7lkysWmiGdlI+G/j+VCElqMCwCFGxrnjz1G0V5MDwiLbn3gPiP6BUqAhRxlZk+6MN8sZsehGcEYqlhbQxMyg==X02o0
  };
  services.cron = {
    enable = true;
    systemCronJobs = [
      "30 22 * * * root sh -c 'shutdown now'"
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
  };
  # nix.settings.auto-optimise-store = true;
  # nix.gc.automatic = true;
  # nix.gc.options = "--delete-older-than 2d";

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  environment.sessionVariables = {
    GTK_DATA_PREFIX = [ "${config.system.path}" ];
  };

  modules.fonts.enable = true;
  fonts.fontconfig = { enable = true; };
  zramSwap.enable = true;
  services.journald.extraConfig = ''
    SystemMaxUse=1G
  '';

  fonts.fontconfig.defaultFonts = {
    monospace = [ "Noto Sans Mono" ];
    sansSerif = [ "Noto Sans" ];
    serif = [ "Noto Sans" ];
  };
  #   environment.etc = {
  #     "docker/daemon.json" = {
  #       text = ''
  # {
  #   "data-root": "/mnt/md127/docker"
  # }
  # '';
  #     };
  #   };
  fonts.fontDir.enable = true;

  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    clang
    openssl
    flameshot
    firefox
    xcompmgr
    # (import (fetchTarball
    #   "https://github.com/aaronjanse/nix-eval-lsp/archive/master.tar.gz"))
    # (import (fetchTarball
    #   "https://github.com/nix-community/rnix-lsp/archive/master.tar.gz"))
    (callPackage /etc/nixos/pkgs/lantern.nix {})
    (callPackage /etc/nixos/pkgs/psiphon.nix { })
    warp
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
    fd
    breeze-gtk
    breeze-qt5
    nixfmt
    gimp
    mpv
    inkscape
    evince
    xorg.xwininfo
    xboxdrv
    mangohud
    pulseaudio
    gnome.gnome-system-monitor
    cabal2nix
    dbeaver
    ccls
    jupyter
    docker-compose
    playerctl
    libusb
    rocketchat-desktop
    tetex
    gnumake
    btop
    calibre
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
    jetbrains.idea-community
    (callPackage /etc/nixos/pkgs/picom-animations.nix { })
    (callPackage /etc/nixos/pkgs/puush-linux.nix { })
    # (pkgs.callPackage /mnt/md127/nixpkgs/pkgs/applications/networking/instant-messengers/telegram/tdesktop { })
    # (pkgs.qt6Packages.callPackage /mnt/md127/nixpkgs/pkgs/applications/networking/instant-messengers/telegram/tdesktop {
    # abseil-cpp = pkgs.abseil-cpp_202111;
    # })
    #(pkgs.callPackage /etc/nixos/pkgs/tdesktop.nix { })
    # (pkgs.callPackage ./pkgs/openhab.nix { })
    #(callPackage ./pkgs/psiphon.nix { })
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
    ntfs3g
    redshift
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
    python39Packages.yt-dlp
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
    (pkgs.callPackage /etc/nixos/pkgs/get_current_screen_geometry.nix { })
    # (pkgs.callPackage /etc/nixos/pkgs/get_current_screen_geometry.nix { })
    # NOTE https://nixos.wiki/wiki/Nixpkgs/Modifying_Packages
    (callPackage /etc/nixos/pkgs/guake-latest.nix { })
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
