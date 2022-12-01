# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, ... }:

{
  imports = [
    ./hardware-configuration.nix
    # <home-manager/nixos>
  ];
  nix = {
    package = pkgs.nixFlakes; # or versioned attributes like nixVersions.nix_2_8
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  user = "andrew";
  virtualisation.anbox.enable = true;
  systemd.user.services.gtk-sni-tray = {
    description = "Gtk sni tray";
    wantedBy = [ "default.target" ];
    serviceConfig = {
      ExecStart =
        "${pkgs.haskellPackages.status-notifier-item}/bin/status-notifier-watcher";
    };
  };

  systemd.user.services.gnome-polkit = {
    description = "Gnome polkit gui";
    wantedBy = [ "multi-user.target" ];
    after = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart =
        "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
    };
  };

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  # hardware.opengl.extraPackages = [ pkgs.amdvlk ];
  virtualisation.spiceUSBRedirection.enable = true;
  #4boot.plymouth.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader.grub.enable = true;
  boot.loader.grub.default = 2;
  boot.loader.grub.version = 2;
  boot.blacklistedKernelModules = ["iTCO_wdt" "iTCO_vendor_support"];
  boot.tmpOnTmpfs = true;
  # boot.cleanTmpDir = true;

  boot.loader.grub.device = "/dev/sda";

  networking.hostName = config.user;

  time.timeZone = "Europe/Moscow";

  virtualisation.docker = {
    enable = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  services.gvfs.enable = true;
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ACTION=="add", ATTR{idVendor}=="0e8d", ATTR{idProduct}=="201d" MODE="0777" GROUP="users"
  '';

  services.xserver = {
    enable = true;
    # videoDrivers = [ "amdgpu" ];

    # Doesnt work
    layout = "us,ru";
    xkbOptions = "grp:alt_shift_toggle";

    libinput = {
      enable = true;
      mouse = { accelProfile = "flat"; };
    };
    exportConfiguration = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager = {
      defaultSession = "none+xmonad";
    };
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  sound.enable = true;
  hardware.bluetooth.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # TODO Move to home manager user config?
  modules.hjkl.enable = true;

  users.defaultUserShell = pkgs.fish;

  users.users.${config.user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

  users.users."${config.user}-work" = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

  nixpkgs.config =
    let nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
    in {
      allowUnfree = true;
      allowBroken = true;
      permittedInsecurePackages = [
        "libdwarf-20181024"
      ];
      packageOverrides = pkgs: {
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
      "30 21 * * * root sh -c 'shutdown now'"
    ];
  };

  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
  programs.seahorse.enable = true;
  services = {
    #gnome.gnome-keyring.enable = true;
    gnome.at-spi2-core.enable = true;
    dbus.enable = true;
  };
  nix.settings.auto-optimise-store = true;
  # nix.gc.automatic = true;
  # nix.gc.options = "--delete-older-than 2d";

  # List packages installed in system profile. To search, run:
  # $ nix search wget

  environment.sessionVariables = {
    GTK_DATA_PREFIX = [ "${config.system.path}" ];
  };

  modules.fonts.enable = true;
  fonts.fontconfig = { enable = true; };
  services.emacs.package = pkgs.emacsNativeComp;
  services.emacs.install = true;
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;
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

  environment.systemPackages =
    with pkgs;
    let unstable = import <nixos-unstable> {}; # https://nixos.wiki/wiki/FAQ#How_can_I_install_a_package_from_unstable_while_remaining_on_the_stable_channel.3F
    in [
      vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      wget
      clang
      openssl
      flameshot
      firefox
      # (import (fetchTarball
      #   "https://github.com/aaronjanse/nix-eval-lsp/archive/master.tar.gz"))
      # (import (fetchTarball
      #   "https://github.com/nix-community/rnix-lsp/archive/master.tar.gz"))
      (callPackage /etc/nixos/pkgs/lantern.nix { })
      (callPackage /etc/nixos/pkgs/psiphon.nix { })
      (callPackage /etc/nixos/pkgs/warp.nix { })
      #cloudflare-warp
      (haskellPackages.callPackage /etc/nixos/modules/taffybar/build/taffybar.nix { })
      tmux
      # Config https://github.com/elken/tabbed/blob/master/config.h
      # pkgs.tabbed.override {
      # customConfig = builtins.readFile ../files/tabbed-config.h;
      # };
      (callPackage /etc/nixos/modules/tabbed/tabbed.nix { })
      git
      ripgrep
      fd
      nixfmt
      gimp
      mpv
      inkscape
      evince
      xorg.xwininfo
      pulseaudio
      cabal2nix
      dbeaver
      ccls
      jupyter
      docker-compose
      playerctl
      libusb
      rocketchat-desktop
      tetex
      btop
      calibre
      brave
      peco
      ffmpeg
      slop
      libnotify
      xclip
      xdotool
      libsForQt5.breeze-gtk
      libsForQt5.breeze-qt5
      pasystray
      pavucontrol
      paprefs
      shotcut
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
      jpegoptim
      chatterino2
      filelight
      x11vnc
      haskellPackages.status-notifier-item
      gnome.dconf-editor
      gnome.gnome-characters
      minidlna
      ntfs3g
      redshift
      gnome.gnome-boxes
      rustdesk
      qbittorrent
      looking-glass-client
      gnome.nautilus
      spice-vdagent
      easyeffects
      evolution
      nodejs
      libreoffice
      koreader
      vlc
      wineWowPackages.stable
      whatsapp-for-linux
      libvirt
      dunst
      android-tools
      python39Packages.yt-dlp
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
      (callPackage /etc/nixos/pkgs/jetbrains-gateway.nix { })
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
