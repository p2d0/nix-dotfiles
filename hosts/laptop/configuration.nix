
{ config, pkgs, self, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];
  modules.flakes.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = [
      # pkgs.xdg-dbus-proxy pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-gnome pkgs.xdg-desktop-portal-kde
      pkgs.xdg-desktop-portal-kde
    ];
  };
  xdg.portal.config.common.default = "*";
  security.rtkit.enable = true;

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;
  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel vaapiVdpau libvdpau-va-gl];
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel ];

  boot.loader.grub.enable = true;
  boot.loader.grub.default = 2;
  boot.loader.grub.version = 2;
  boot.blacklistedKernelModules = [ "iTCO_wdt" "iTCO_vendor_support" ];

  boot.tmpOnTmpfs = true;

  # TODO boot.loader.grub.device = "/dev/sda";

  # TODO extract
  networking.hostName = config.user;

  time.timeZone = "Europe/Moscow";

  i18n.defaultLocale = "en_US.UTF-8";
  services.gvfs.enable = true;
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ACTION=="add", ATTR{idVendor}=="0e8d", ATTR{idProduct}=="201d" MODE="0777" GROUP="users"
    KERNEL=="ttyUSB*", MODE="0666"
  '';

  services.xserver = {
    enable = true;

    layout = "us,ru";
    xkbOptions = "grp:alt_shift_toggle";

    libinput = {
      enable = true;
      mouse = { accelProfile = "flat"; };
    };
    exportConfiguration = true;
    windowManager.i3.enable = true;

    displayManager = {
      defaultSession = "none+i3";
      autoLogin = {
        enable = true;
        user = config.user;
      };
    };
  };
  hardware.bluetooth.enable = true;
  modules.hjkl.enable = true;
  modules.keyrings.enable = true;

  users.users.${config.user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

  users.defaultUserShell = pkgs.fish;

  modules.emacs-with-doom  =
    {
      enable = true;
      package = pkgs.emacs;
    };

  services = {
    dbus = {
      enable = true;
      packages = [];
    };
    cron = {
      enable = true;
      systemCronJobs = [
        # "00 20 * * * andrew fish -c 'sync_repos'"
      ];
    };
  };

  nix.settings.auto-optimise-store = true;
  nix.settings.trusted-users = [ "root" "andrew" ];
  nix.gc.automatic = true;
  nix.gc.options = "--delete-older-than 1d";

  systemd.coredump.extraConfig = ''
    Storage=none
  '';

  modules.xdg.sessionVariables = true;
  modules.fonts.enable = true;
  modules.timed-shutdown.enable = false;
  modules.timed-shutdown.time = "23:00:00";

  modules.darkman.enable = true;
  modules.vpn.enable = true;
  modules.vm.enable = false;

  zramSwap.enable = true;

  programs.java = {
    enable = true;
    package = pkgs.oraclejre8;
  };
  environment.systemPackages = with pkgs;
    [
      vim
      wget
      flameshot
      unstable.firefox
      killall
      xdo
      inotify-tools
      my.pythonbin
      neovim
      git
      ripgrep
      fd
      # TODO THEME
      breeze-gtk
      breeze-qt5
      nixfmt
      gimp
      mpv
      playerctl
      libusb1
      ffmpeg
      peco
      xclip
      xdotool
      tldr
      # TODO THEME
      libsForQt5.breeze-gtk
      libsForQt5.breeze-qt5
      pasystray
      pavucontrol
      paprefs
      discord
      speedcrunch
      unstable-small.tdesktop
      jpegoptim
      filelight
      gnome.gedit
      qbittorrent
      gnome.nautilus
      zip
      inetutils
      xsettingsd
      nodejs
      # TODO THEME
      iconpack-obsidian
      gsettings-desktop-schemas
      dunst
      unstable.python39Packages.yt-dlp
      python39Packages.virtualenv
      python39Packages.pip
      feh
      gnome.eog
      htop
      unzip
      my.get_current_screen_geometry
      my.guake-latest
    ];
  services.openssh.enable = true;
  services.openssh.startWhenNeeded = false;

  networking.firewall.enable = false;
  system.stateVersion = "21.11"; # Did you read the comment?

}
