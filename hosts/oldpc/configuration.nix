{ config, pkgs, self, ... }:

{
  imports = [
    ./hardware-configuration.nix
  ];
  modules.flakes.enable = true;

  # users.users.andrew.extraGroups = ["corectrl" "gamemode"];
  # programs.corectrl ={
  #   enable = true;
  # };
  security.rtkit.enable = true;

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [];

    extraPackages32 = with pkgs.driversi686Linux; [];
  };

  environment.variables = { ROC_ENABLE_PRE_VEGA = "1"; };

  # hardware.opengl.enable = true;
  # hardware.opengl.driSupport = true;
  # hardware.opengl.extraPackages = with pkgs; [ intel-vaapi-driver libva-vdpau-driver libvdpau-va-gl];
  # hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ intel-vaapi-driver ];

  environment.variables = {
    LIBVA_DRIVER_NAME = "i965";
  };
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.grub.enable = true;
  # boot.loader.grub.default = 2;
  # boot.loader.grub.version = 2;
  boot.blacklistedKernelModules = [ "iTCO_wdt" "iTCO_vendor_support" ];

  # boot.kernelPackages = pkgs.linuxPackages_lts;

  # TODO just use self.user 
  user = self.user;

  boot.tmpOnTmpfs = true;
  boot.tmp.cleanOnBoot = true;

  # TODO boot.loader.grub.device = "/dev/sda";

  # TODO extract
  networking.hostName = "${config.user}";
  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Moscow";

  services.gvfs.enable = true;
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ACTION=="add", ATTR{idVendor}=="0e8d", ATTR{idProduct}=="201d" MODE="0777" GROUP="users"
    KERNEL=="ttyUSB*", MODE="0666"
  '';

  services.xserver = {
    enable = true;
    videoDrivers = [ "amdgpu" ];
    dpi = 96;

    # Doesnt work
    xkb = {
      layout = "us,ru";
      options = "grp:alt_shift_toggle,compose:ralt";
    };
    serverFlagsSection = ''
  Option "BlankTime" "1"
  Option "StandbyTime" "1"
  Option "SuspendTime" "1"
  Option "OffTime" "1"
'';
    exportConfiguration = true;
  };

  programs.tmux = {
    enable = true;
    extraConfig = ''
set-window-option -g mode-keys vi

unbind C-b
set-option -g prefix C-s
bind-key C-s send-prefix

# act like vim
setw -g mode-keys vi
bind-key h select-pane -L
bind-key j select-pane -D
bind-key k select-pane -U
bind-key l select-pane -R

set-option -g set-titles on
set-option -g set-titles-string "#T"
bind-key -T copy-mode-vi v send -X begin-selection


run-shell ${pkgs.tmuxPlugins.yank}/share/tmux-plugins/yank/yank.tmux
'';
  };

  modules.maestral.enable = true;
  modules.mihomo = {
    enable = true;
    configFile = "/mnt/md127/Dropbox/mihomo/config.yaml";
    # package = pkgs.unstable.mihomo;
    package = pkgs.my.mihomo;
    # package = pkgs.old-24-05.mihomo;
    tunMode = true;
  };

  modules.hypr = {
    enable = true;
    settingsFile = "/etc/nixos/configs/hypr/hyprland-laptop.conf";
  };

  services.displayManager = {
    enable = true;
    defaultSession = "hyprland";

    sddm.enable = true;
    sddm.package = pkgs.kdePackages.sddm;
    sddm.settings = {
      X11 = {
        ServerArguments="-s 1 -logfile /tmp/x111.log";
      };
    };
    sddm.extraPackages = [
      pkgs.sddm-astronaut
    ];
    sddm.wayland.enable = false;
    sddm.wayland.compositorCommand =
      let
        xcfg = config.services.xserver;
        westonIni = (pkgs.formats.ini { }).generate "weston.ini" {
          libinput = {
            enable-tap = config.services.libinput.mouse.tapping;
            left-handed = config.services.libinput.mouse.leftHanded;
          };
          core = {
            idle-time = 15;
          };
          keyboard = {
            keymap_model = xcfg.xkb.model;
            keymap_layout = xcfg.xkb.layout;
            keymap_variant = xcfg.xkb.variant;
            keymap_options = xcfg.xkb.options;
          };
        };
      in
        "${pkgs.lib.getExe pkgs.weston} --idle-time=5 --shell=kiosk -c ${westonIni}";
    sddm.theme = "sddm-astronaut-theme";
    # enable = true;
    # defaultSession = "Hyprland";
    # ly.enable = true;
    # autoLogin = {
    #   enable = true;
    #   user = config.user;
    # };
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;
  modules.hjkl.enable = true;
  modules.keyrings.enable = true;

  users.users.${config.user} = {
    isNormalUser = true;
    extraGroups = [ "corectrl" "gamemode" "wheel" "docker" ]; # Enable ‘sudo’ for the user.
  };

  users.defaultUserShell = pkgs.fish;
  programs.fish.enable = true;

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

  systemd.services.nix-daemon.serviceConfig = {
    MemoryMax = "2G"; 
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
  modules.guake.enable = true;
  modules.timed-shutdown.enable = false;
  modules.timed-shutdown.time = "23:00:00";

  modules.darkman.enable = true;
  modules.vpn.enable = true;
  modules.vm.enable = false;
  modules.firefox.enable = true;

  zramSwap.enable = true;
  zramSwap.memoryPercent = 70;

  # programs.java = {
  #   enable = true;
  #   package = pkgs.oraclejre8;
  # };
  environment.systemPackages = with pkgs;
    [
      vim
      wget
      flameshot
      killall
      xdo
      inotify-tools
      (pkgs.python3.withPackages (ps: [
        ps.python-miio
        ps.pygobject-stubs
        ps.requests
        ps.pygobject3
        ps.lxml
      ]))
      my.pythonbin
      neovim
      gnome-disk-utility
      git
      ripgrep
      fd
      # TODO THEME
      kdePackages.breeze-gtk
      # libsForQt5.breeze-qt5
      nixfmt
      gimp
      mpv
      playerctl
      brave
      gnome-system-monitor
      kdePackages.kdenlive
      libusb1
      ffmpeg
      peco
      xclip
      mc
      ranger
      xdotool
      tldr
      wofi
      # TODO THEME
      # libsForQt5.breeze-gtk
      # libsForQt5.breeze-qt5
      pasystray
      pavucontrol
      cachix
      # deskflow
      paprefs
      # discord
      speedcrunch
      telegram-desktop
      # unstable-small.tdesktop
      jpegoptim
      # filelight
      gedit
      qbittorrent
      nautilus
      zip
      inetutils
      xsettingsd
      intel-gpu-tools
      nodejs
      # TODO THEME
      iconpack-obsidian
      gsettings-desktop-schemas
      dunst
      # unstable.pythonPackages.yt-dlp
      # python39Packages.virtualenv
      # python39Packages.pip
      feh
      eog
      htop
      unzip
      my.get_current_screen_geometry
      # my.guake-latest
    ];
  services.openssh.enable = true;
  services.openssh.startWhenNeeded = false;

  networking.firewall.enable = false;
  system.stateVersion = "25.11"; # Did you read the comment?
}
