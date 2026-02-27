{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs;
    [
      unstable.yt-dlp
      nix-index
      gcalcli
      sshfs
      libnotify
      cachix
      vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      kdePackages.kdenlive
      my.pythonbin
      unstable.video-trimmer
      git
      ripgrep
      wmctrl
      kdePackages.breeze-gtk
      fd
      unstable.gimp3
      mpv
      jq
      nix-tree
      libva-utils
      wofi
      docker-compose
      playerctl
      libusb1
      peco
      ffmpeg-full
      my.puush-linux
      speedcrunch
      songrec
      # unstable.nixd
      ntfs3g
      nautilus
      nautilus-python
      alacritty
      htop
      my.get_current_screen_geometry
      unzip
      (sddm-astronaut.override {
        embeddedTheme = "black_hole";
        # themeConfig = {
        #   # FullBlur = true;
        #   # BlurRadius = 25;
        #   # PasswordFocus = false;
        # };
      })
    ];

  modules.timed-lock.enable = true;
  modules.gpu-screen-recorder.enable = true;
  services.libinput = {
    enable = true;
    mouse = { accelProfile = "flat"; };
  };
  i18n = {
    defaultLocale = "en_US.UTF-8";
    # extraLocaleSettings = {
    #   LC_MEASUREMENT = "en_SE.UTF-8";
    #   LC_NUMERIC = "en_SE.UTF-8";
    #   # For dates formatted like ISO8601
    #   LC_TIME = "en_SE.UTF-8";
    # };
    supportedLocales = [ "all" ];
  };

  services = {
    cron = {
      enable = true;
      systemCronJobs = [
        "00 20 * * * andrew fish -c 'sync_repos'"
      ];
    };
  };
  
  networking.extraHosts = ''
    130.255.77.28 ntc.party
  '';
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
  modules.hypr.enable = true;

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
        }
      ];
      groups = [ "wheel" ];
    }];
  };
  programs.fish.enable = true;
  users.defaultUserShell = pkgs.fish;
  modules.emacs-with-doom  =
    {
      enable = true;
      package = pkgs.emacs30-pgtk;
    };
  services.speechd.enable = false;
  programs.tmux = {
    enable = true;
    escapeTime = 0;
    keyMode = "vi";
    terminal = "screen-256color";
    extraConfig = ''

set-option -g status-interval 5
set-option -g automatic-rename on
# set-option -g automatic-rename-format "#{pane_current_command}"
set-window-option -g allow-rename on

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

  modules.mihomo = {
    enable = true;
    configFile = "/home/${config.user}/Dropbox/mihomo/config.yaml";
    # package = pkgs.unstable.mihomo;
    package = pkgs.my.mihomo;
    # package = pkgs.old-24-05.mihomo;
    tunMode = true;
  };

  modules.fonts.enable = true;
  modules.guake.enable = true;
  modules.darkman.enable = true;
  modules.vpn.enable = true;
  modules.firefox.enable = true;
  environment.etc = {
    "wireplumber/policy.lua.d/11-bluetooth-policy.lua".text = ''
      bluetooth_policy.policy["media-role.use-headset-profile"] = false
    '';
    "docker/daemon.json" = {
      text = ''
        {
        "log-driver": "json-file",
        "log-opts": {"max-size": "10m", "max-file": "3"}
        }
      '';
    };
  };

  services.journald.extraConfig = ''
    SystemMaxUse=2G
  '';

  environment.pathsToLink = [
    "/share/nautilus-python/extensions"
  ];

  environment.sessionVariables."NAUTILUS_4_EXTENSION_DIR" = "${config.system.path}/lib/nautilus/extensions-4";
  environment.sessionVariables."NAUTILUS_EXTENSION_DIR" = "${config.system.path}/lib/nautilus/extensions-4";


}
