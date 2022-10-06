# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./modules/hjkl/hjkl.nix
    ./andrew.nix
    ./andrew-work.nix
    ./modules/options.nix
    ./modules/pipewire.nix
    <home-manager/nixos>
  ];
  nix = {
    package = pkgs.nixFlakes; # or versioned attributes like nixVersions.nix_2_8
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  user = "andrew";

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
  # boot.plymouth.enable = true;

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

  virtualisation.docker.enable = true;

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
      packageOverrides = pkgs: {
        pr181605 = import (fetchTarball
          "${nixpkgs-tars}7cc979502c3dc5480ef3e4ffe1a05c897084d34b.tar.gz") {
            config = config.nixpkgs.config;
          };
        master = import (fetchTarball
          "${nixpkgs-tars}master.tar.gz") {
            config = config.nixpkgs.config;
          };
      };
    };

  services.blueman.enable = true;
  programs.dconf.enable = true;

  services.udev.packages = with pkgs; [ gnome.gnome-settings-daemon ];
  programs.seahorse.enable = true;
  services = {
    #gnome.gnome-keyring.enable = true;
    gnome.at-spi2-core.enable = true;
    dbus.enable = true;
  };
  # nix.settings.auto-optimise-store = true;
  # nix.gc.automatic = true;
  # nix.gc.options = "--delete-older-than 2d";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  fonts.fonts = with pkgs; [
    pkgs.jetbrains-mono
    pkgs.font-awesome
    pkgs.freefont_ttf
    pkgs.nerdfonts
    pkgs.weather-icons
    pkgs.fantasque-sans-mono
    pkgs.comfortaa
    pkgs.arphic-uming
    pkgs.source-han-code-jp
    pkgs.baekmuk-ttf
    pkgs.ipafont
    pkgs.noto-fonts-cjk-sans
    pkgs.noto-fonts-emoji
    pkgs.noto-fonts
    pkgs.noto-fonts-extra
    pkgs.fira-code
    pkgs.hanazono
    pkgs.dejavu_fonts
    pkgs.material-design-icons
    pkgs.material-icons
  ];

  environment.sessionVariables = {
    GTK_DATA_PREFIX = [ "${config.system.path}" ];
  };

  fonts.fontconfig = { enable = true; };
  services.emacs.package = pkgs.emacsNativeComp;
  services.emacs.install = true;
  services.emacs.enable = true;
  services.emacs.defaultEditor = true;
  zramSwap.enable = true;

  fonts.fontconfig.defaultFonts = {
    monospace = [ "Noto Sans Mono" ];
    sansSerif = [ "Noto Sans" ];
    serif = [ "Noto Sans" ];
  };

  fonts.fontDir.enable = true;

  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    clang
    openssl
    pr181605.kdiskmark
    flameshot
    firefox
    (import (fetchTarball
      "https://github.com/aaronjanse/nix-eval-lsp/archive/master.tar.gz"))
    (import (fetchTarball
      "https://github.com/nix-community/rnix-lsp/archive/master.tar.gz"))
    (callPackage ./pkgs/lantern.nix { })
    (callPackage ./pkgs/psiphon.nix { })
    (haskellPackages.callPackage ./modules/taffybar/build/taffybar.nix { })
    tmux
    git
    ripgrep
    fd
    nixfmt
    gimp
    mpv
    evince
    xorg.xwininfo
    pulseaudio
    libusb
    rocketchat-desktop
    tetex
    btop
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
    (callPackage ./pkgs/picom-animations.nix { })
    (callPackage ./pkgs/puush-linux.nix { })
    # (pkgs.callPackage /mnt/md127/nixpkgs/pkgs/applications/networking/instant-messengers/telegram/tdesktop { })
    # (pkgs.qt6Packages.callPackage /mnt/md127/nixpkgs/pkgs/applications/networking/instant-messengers/telegram/tdesktop {
    # abseil-cpp = pkgs.abseil-cpp_202111;
    # })
    #(pkgs.callPackage ./pkgs/tdesktop.nix { })
    # (pkgs.callPackage ./pkgs/openhab.nix { })
    #(callPackage ./pkgs/psiphon.nix { })
    speedcrunch
    discord
    master.tdesktop
    jpegoptim
    chatterino2
    filelight
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
    vlc
    wineWowPackages.stable
    whatsapp-for-linux
    libvirt
    dunst
    android-tools
    python39Packages.yt-dlp
    feh
    alacritty
    dmenu
    gnome.gnome-disk-utility
    cabal2nix
    htop
    # (pkgs.callPackage ./pkgs/get_current_screen_geometry.nix { })
    # (pkgs.callPackage ./pkgs/get_current_screen_geometry.nix { })
    # NOTE https://nixos.wiki/wiki/Nixpkgs/Modifying_Packages
    (callPackage ./pkgs/guake-latest.nix { })
    (callPackage ./pkgs/jetbrains-gateway.nix { })
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
