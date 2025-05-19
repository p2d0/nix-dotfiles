# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, self, ... }:

{
  imports = [ ./hardware-configuration.nix ];
  modules.flakes.enable = true;
  modules.maestral.enable = true;
  modules.gpu-screen-recorder.enable = true;

  # TODO Agenix
  # https://nixos.wiki/wiki/Agenix
  user = self.user;

  qt.platformTheme = "gnome";
  # xdg.portal = {
  #   enable = true;
  #   extraPortals =
  #     [ # pkgs.xdg-dbus-proxy pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-gnome pkgs.xdg-desktop-portal-kde
  #       # https://superuser.com/questions/944119/replace-gtk-file-dialog-with-alternative
  #       # Custom file dialog for browsers, etc.
  #       # pkgs.xdg-desktop-portal-kde
  #     ];
  #   # gtkUsePortal = true;
  #   # https://github.com/NixOS/nixpkgs/pull/179204
  #   # environment.sessionVariables.GTK_USE_PORTAL
  #   # xdgOpenUsePortal = true;
  # };
  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    # wlr.enable = true;
    # I think the name under config has to match up with the value of XDG_CURRENT_DESKTOP.
    # kde portal by default, fallback to anything for KDE and i3
    # I don't set this for KDE in my personal config and it seems to work fine.
    # config.KDE.default = [ "kde" "*" ];
    # config.${"none+i3"}.default = [ "kde" "*" ];
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-kde
      # pkgs.xdg-desktop-portal-wlr
    ];
  };
  xdg.portal.config.common.default = "*";
  nix.settings.trusted-users = [ "root" "andrew" ];

  security.rtkit.enable = true;
  security.pki.certificateFiles = [
    (builtins.fetchurl {
      url = "https://gu-st.ru/content/lending/russian_trusted_root_ca_pem.crt";
    })
    (builtins.fetchurl {
      url = "https://gu-st.ru/content/lending/russian_trusted_sub_ca_pem.crt";
    })
  ];

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    zlib # numpy
    # glib
    # glibc
    # libgcc
    # sqlalchemy
    # libcxxStdenv
    # gcc12.cc

    # gcc12.cc.lib
    # gcc.cc.libgcc.lib
    # that's where the shared libs go, you can find which one you need using
    # nix-locate --top-level libstdc++.so.6  (replace this with your lib)
    # ^ this requires `nix-index` pkg
  ];

  hardware.graphics.enable = true;
  hardware.graphics.extraPackages = with pkgs; [
    ocl-icd
    # vaapiVdpau
    # libvdpau-va-gl
    # amdvlk
    # libva1
    # libva
    # rocm-opencl-icd
    # rocm-opencl-runtime
  ];
  # systemd.tmpfiles.rules =
  #   [ "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}" ];
  # rocmTargets = ["gfx803"];
  # hardware.graphics.extraPackages32 = with pkgs.pkgsi686Linux; [ vaapiIntel ];
  services.ratbagd.enable = true;

  environment.variables = { ROC_ENABLE_PRE_VEGA = "1"; };

  boot.kernelPackages = pkgs.unstable.linuxPackages_zen;

#  boot.loader.systemd-boot.enable = false;
#  boot.loader.grub.useOSProber = true;
#  boot.loader.grub.enable = true;
#  boot.loader.grub.default = 1;
#  boot.loader.grub.efiSupport = true;
#  boot.loader.grub.device = "nodev";


 boot.loader.efi = {
   canTouchEfiVariables = true;
   efiSysMountPoint = "/boot/efi";
 };

  boot.blacklistedKernelModules = [ "iTCO_wdt" "iTCO_vendor_support" ];

  # boot.tmp.useTmpfs = true;
  boot.tmp.cleanOnBoot = true;



  networking.hostName = config.user;
  # networking.proxy.default = "http://localhost:8092/";

  time.timeZone = "Europe/Moscow";
  boot.extraModprobeConfig = ''
    options usbhid mousepoll=1
  '';

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  # i18n.defaultLocale = "en_US.UTF-8";
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

  services.gvfs.enable = true;
  # services.gvfs.package = pkgs.gvfspkgs.gnome.gvfs;
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ACTION=="add", ATTR{idVendor}=="0e8d", ATTR{idProduct}=="201d" MODE="0777" GROUP="users"
    KERNEL=="hidraw*", MODE="0666"
    KERNEL=="hiddev*", MODE="0666"
    KERNEL=="ttyUSB*", MODE="0666"
  '';

  # users.users.andrew.extraGroups = ["corectrl" "gamemode"];
  # programs.corectrl ={
  #   enable = true;
  # };

  programs.droidcam.enable = true;
  # programs.sway = {
  #   enable = true;
  #   wrapperFeatures.gtk = true;
  # };
  services.libinput = {
    enable = true;
    mouse = { accelProfile = "flat"; };
  };


  hardware.nvidia = {
    modesetting.enable = true;
    nvidiaSettings = true;
    # package = pkgs.unstable.linuxKernel.packages.linux_zen.nvidia_x11;
    powerManagement = {
      enable = true;
      finegrained = false;
    };
    open = true;
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "nvidia" # "amdgpu"
                   ];
    dpi = 96;
    xrandrHeads = [
      # {
      #   output = "DP-4";
      #   primary = true;
      #   monitorConfig = ''
      #     Modeline "2560x1080_75.00"  294.00  2560 2744 3016 3472  1080 1083 1093 1130 -hsync +vsync
      #     Modeline "2560x1080_74.99"  294.00  2560 2744 3016 3472  1080 1083 1093 1130 -hsync +vsync
      #     Modeline "2560x1080@75"  228.25  2560 2608 2640 2720  1080 1083 1093 1119 +hsync -vsync
      #     Option "PreferredMode" "2560x1080@75"'';
      # }
      # {
      #   output = "DVI-D-0";
      #   monitorConfig = ''
      #     Option "Position" "2560 0"
      #     Option          "TearFree" "true"
      #   '';
      # }
      # {
      #         output = "HDMI-A-0";
      #         monitorConfig = ''
      #          Modeline "2000x1000_60.00"  166.25  2000 2128 2336 2672  1000 1003 1013 1038 -hsync +vsync
      #          Option "PreferredMode" "2000x1000_60.00"
      #          Option "Position" "0 1080"
      # '';
      #       }
    ];

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
    #  deviceSection = ''
    #      Option          "TearFree" "true"
    # '';
    exportConfiguration = true;
    windowManager.i3 = {
      enable = true;
      package = pkgs.unstable.i3;
      # .overrideAttrs(oldAttrs: rec {
      #   src = /mnt/md127/i3;
      #   dontCheck = true;
      #   doCheck = false;
      # })
    };
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

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware.bluetooth.enable = true;

  modules.plymouth.enable = true;
  modules.gamemode.enable = true;
  modules.silentboot.enable = true;
  modules.hypr.enable = true;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # TODO Move to home manager user config?
  modules.sway.enable = false;
  modules.ilzabot.enable = false;
  modules.hjkl.enable = false;
  modules.printing3d.enable = true;
  modules.warp.enable = false;
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

  services.transmission = {
    enable = false;
    settings.incomplete-dir-enabled = false;
    home = "/mnt/old/transmission";
  };

  services.blueman.enable = true;
  programs.dconf.enable = true;

  services = {
    #gnome.gnome-keyring.enable = true;
    dbus = {
      enable = true;
      packages = [ ];
    };
    # xrdp.enable = true;
    # xrdp.defaultWindowManager = "dbus-launch --exit-with-session;i3;";
    cron = {
      enable = true;
      systemCronJobs = [
        # "30 20 * * * andrew fish -c 'update-system'"
        "00 20 * * * andrew darkman set dark"
        "00 23 * * * andrew DBUS_SESSION_BUS_ADDRESS='unix:path=/run/user/1000/bus' /run/current-system/sw/bin/notify-send 'Sleep'"
        "00 21 * * * root /etc/nixos/shutdown.sh"
        "55 21 * * * andrew DBUS_SESSION_BUS_ADDRESS='unix:path=/run/user/1000/bus' /run/current-system/sw/bin/notify-send 'Shutdown in 5 MINUTES'"
        "00 20 * * * andrew /etc/nixos/update_ip.sh"
        # "00 23 * * * andrew /home/andrew/Dropbox/miner_enable.sh"
        # "00 7 * * * andrew /home/andrew/Dropbox/miner_disable.sh"
        # "35 6 * * * root /etc/nixos/play_alsa_alarm.sh"
        "30 7 * * * root /etc/nixos/stop_alarm.sh"
        "00 20 * * * andrew fish -c 'sync_repos'"
        "0,5,10,15,20,25,30,35,40,45,50,55 * * * * andrew sleep 12 ; wget --no-check-certificate -O - https://freedns.afraid.org/dynamic/update.php?RnBTMHFiQlhHWnVmUXpNYmtLWlQ0ZXB5OjIyMTIzNjM3 >> /tmp/freedns_ug_kyrgyzstan_kg.log 2>&1 &"
      ];
    };
  };

  nix.settings.auto-optimise-store = true;
  nix.gc.automatic = false;
  nix.gc.options = "--delete-older-than 1d";

  security.polkit.extraConfig = ''
polkit.addRule(function(action, subject) {
    if (action.id == "org.corectrl.helper.init" &&
        subject.user == "${config.user}") {
        return polkit.Result.YES;
        }
});

polkit.addRule(function(action, subject) {
    if (action.id == "org.corectrl.helper.init" &&
        subject.user == "${config.user}") {
        return polkit.Result.YES;
        }
});
'';

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # services.nginx = {
  #   enable = false;
  #   recommendedGzipSettings = true;
  #   recommendedOptimisation = true;
  #   # recommendedProxySettings = true;
  #   recommendedTlsSettings = true;
  #   # other Nginx options
  #   virtualHosts."localhost" = {
  #     locations."/" = {
  #       proxyPass = "http://localhost:8989/";
  #       proxyWebsockets = true; # needed if you need to use WebSocket
  #       # extraConfig =
  #       #   "proxy_set_header Host $host;" +
  #       #   # required when the target is also TLS server with multiple hosts
  #       #   "proxy_ssl_server_name on;" +
  #       #   # required when the server wants to use HTTP Authentication
  #       #   "proxy_pass_header Authorization;";
  #       extraConfig =
  #         "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
  #         + "proxy_set_header Host $host;"
  #         + "proxy_set_header X-Forwarded-Proto https;" + "proxy_redirect off;";
  #     };
  #   };
  #   virtualHosts."ug.kyrgyzstan.kg" = {
  #     # sslCertificate = "/etc/letsencrypt/live/ug.kyrgyzstan.kg/fullchain.pem";
  #     # sslCertificateKey = "/etc/letsencrypt/live/ug.kyrgyzstan.kg/privkey.pem";
  #     # TODO Fix ACME
  #     enableACME = false;
  #     forceSSL = false;
  #     # locations."/.well-known/acme-challenge" = {
  #     #   root = "/var/lib/acme/.challenges";
  #     # };
  #     locations."/landing/" = {
  #       alias = "/mnt/md127/upgrade/website2";
  #       tryFiles = "$uri $uri/ /index.html";
  #     };
  #     locations."/" = {
  #       proxyPass = "http://localhost:8989/";
  #       proxyWebsockets = true; # needed if you need to use WebSocket
  #       # extraConfig =
  #       #   "proxy_set_header Host $host;" +
  #       #   # required when the target is also TLS server with multiple hosts
  #       #   "proxy_ssl_server_name on;" +
  #       #   # required when the server wants to use HTTP Authentication
  #       #   "proxy_pass_header Authorization;";
  #       extraConfig =
  #         "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
  #         + "proxy_set_header Host $host;"
  #         + "proxy_set_header X-Forwarded-Proto https;" + "proxy_redirect off;";
  #     };

  #     locations."/wb/" = {
  #       alias = "/mnt/md127/wbcourier/";
  #       tryFiles = "$uri $uri/ /index.html";
  #     };
  #     locations."/dostavista/" = {
  #       proxyPass = "http://localhost:3000/";
  #       # proxyWebsockets = true; # needed if you need to use WebSocket
  #       # extraConfig =
  #       #   "proxy_set_header Host $host;" +
  #       #   # required when the target is also TLS server with multiple hosts
  #       #   "proxy_ssl_server_name on;" +
  #       #   # required when the server wants to use HTTP Authentication
  #       #   "proxy_pass_header Authorization;";
  #       extraConfig = ''
  #         proxy_set_header Host $host;
  #         proxy_cache_bypass $http_upgrade;
  #         proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
  #         proxy_set_header X-Forwarded-Proto $scheme;
  #         proxy_set_header X-Real-IP $remote_addr;
  #         proxy_set_header X-Forwarded-Host $host;
  #         proxy_set_header X-Forwarded-Port $server_port;
  #         proxy_redirect off;
  #         rewrite /dostavista/(.*) /$1 break;
  #       '';
  #     };
  #   };

  #   # virtualHosts."upgradegamma.ru" =  {
  #   #   enableACME = true;
  #   #   forceSSL = true;
  #   #   locations."/" = {
  #   #     proxyPass = "http://localhost:8001/";
  #   #     proxyWebsockets = true;
  #   #     extraConfig =
  #   #       "proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"+
  #   #       "proxy_set_header Host $host;"+
  #   #       "proxy_set_header X-Forwarded-Proto https;"+
  #   #       "proxy_redirect off;";
  #   #   };
  #   # };
  # };
  # security.acme.certs."ug.kyrgyzstan.kg" = {
  #   webroot = "/var/lib/acme/.challenges";
  #   email = "cerkin-3@yandex.ru";
  #   # Ensure that the web server you use can read the generated certs
  #   # Take a look at the group option for the web server you choose.
  #   group = "nginx";
  #   # Since we have a wildcard vhost to handle port 80,
  #   # we can generate certs for anything!
  #   # Just make sure your DNS resolves them.
  #   # extraDomainNames = [ "mail.example.com" ];
  # };

  # users.users.nginx.extraGroups = [ "acme" ];
  # users.users.nginx.group = "nginx";
  # users.groups.nginx = {};

  # services.getty.autologinUser = config.user;
  # services.gretd.enable = true;

  # systemd.coredump.extraConfig = ''
  #   Storage=none
  # '';


  # modules.xdg-variables.sessionVariables = true;

  # programs.java = {
  #   enable = true;
  #   package = pkgs.openjdk8;
  #   # https://javadl.oracle.com/webapps/download/GetFile/1.8.0_281-b09/89d678f2be164786b292527658ca1605/linux-i586/jdk-8u281-linux-x64.tar.gz
  #   # TODO direct link
  # };

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
  programs.proxychains = {
    enable = true;
    proxies.xray = {
      enable = true;
      type = "http";
      host = "127.0.0.1";
      port = 8093;
    };
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "cerkin-3@yandex.ru";
    # certs."mx1.example.org" = {
    # };
  };

  modules.mihomo = {
    enable = true;
    configFile = "/home/${config.user}/Dropbox/mihomo/config.yaml";
    package = pkgs.unstable.mihomo;
    # package = pkgs.old-24-05.mihomo;
    tunMode = true;
  };

  # services.keyd = {
  #   enable = true;
  #   keyboards.default = {
  #     ids = ["*"];
  #     settings = {
  #       main = {
  #         capslock = "overload(hjkl, capslock)";
  #       };
  #       hjkl = {
  #         h = "left";
  #         j = "down";
  #         k = "up";
  #         l = "right";
  #         left = "macro(←)";
  #         right = "macro(→)";
  #         down = "macro(↓)";
  #         up = "macro(↑)";
  #       };
  #     };
  #   };
  # };

  modules.trex.enable = true;
  modules.singbox.enable = false;
  modules.amnezia.enable = false;
  modules.fonts.enable = true;
  modules.guake.enable = true;
  modules.byedpi.enable = false;
  modules.timed-shutdown.enable = false;
  modules.timed-shutdown.time = "23:00:00";
  modules.darkman.enable = true;
  modules.vpn.enable = true;
  modules.vm.enable = false;
  zramSwap.enable = true;
  # zramSwap.writebackDevice = "/dev/sdb1";
  services.journald.extraConfig = ''
    SystemMaxUse=2G
  '';

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
#  environment.extraInit = ''
#xset dpms 15 15 15
#'';
  environment.sessionVariables.NAUTILUS_4_EXTENSION_DIR = "${config.system.path}/lib/nautilus/extensions-4";

  environment.pathsToLink = [
    "/share/nautilus-python/extensions"
  ];

  # environment.sessionVariables.NAUTILUS_EXTENSION_DIR = "${config.system.path}/lib/nautilus/extensions-4";
  modules.taffybar.enable = false;
  environment.systemPackages = with pkgs;
    [
      (pkgs.python3.withPackages (ps: [
        ps.python-lsp-server
        ps.python-telegram-bot
        ps.python-dotenv
        ps.pytest
        ps.python-miio
        ps.pygobject-stubs
        ps.debugpy
        ps.requests
        ps.epc
        ps.pygobject3
        ps.lxml
        ps.tld
        ps.sexpdata
        ps.pyqt6
        ps.pyqt6-sip
        ps.pyqt6-webengine
        ps.pygetwindow
        ps.matplotlib
      ]))

      # my.hbctool
      vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
      OSCAR
      cachix
      (sddm-astronaut.override {
        themeConfig = {
          FullBlur = true;
          BlurRadius = 25;
          PasswordFocus = false;
        };
      })
      poetry
      putty
      texliveMedium
      nix-index
      sshfs
      # mattermost-desktop
      # charles
      gucharmap
      autokey
      alsa-utils
      wgcf
      # my.amneziawg-tools
      # my.amneziawg-go
      wget
      unstable.rnnoise-plugin

      clang
      file
      clojure
      kdenlive
      lm_sensors
      # mihomo
      openssl
      nodePackages.localtunnel
      # pr218037.microsoft-edge-dev
      gcc.cc.libgcc
      gcc.cc.libgcc.lib
      keepassxc
      remmina
      unstable.aider-chat
      unstable.unarc
      pkgs.libayatana-appindicator
      pkgs.libayatana-appindicator-gtk3
      conda
      # megasync
      # unstable.jetbrains.idea-community
      unstable.gamescope
      create-react-app
      my.cursor
      parallel
      vscode

      yandex-disk
      gpu-screen-recorder
      shared-mime-info
      clinfo
      radeontop
      qrencode
      unrar
      # pr229886.amdgpu-pro-libs.amf
      # pr229886.amdgpu-pro-libs.vulkan
      # pr229886.amdgpu-pro-libs.opengl
      # pr229886.amdgpu-pro-libs.prefixes
      # unstable.amf-headers
      # pr314293.tdlib
      nixfmt-classic
      gnome-themes-extra
      # python2
      xmrig
      sumneko-lua-language-server
      tgs2png
      my.tg
      luarocks
      helvum
      # unstable.cloudflare-warp
      nmap
      # skypeforlinux
      neovide
      # my.hiddify
      lazygit
      ranger
      mesa
      # gmsh
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
      (flameshot.override {
        enableWlrSupport = true;
      })
      unstable.flyctl
      # .overrideAttrs(oldAttrs: rec {
      #   NIX_CFLAGS_COMPILE = "-DUSE_WAYLAND_CLIPBOARD";
      #   CFLAGS = ["-DUSE_WAYLAND_CLIPBOARD"];
      #   # configureFlags = [
      #   #   "CPPFLAGS=-DUSE_WAYLAND_CLIPBOARD";
      #   # ];
      # })
      pciutils
      # musescore
      # deluge
      usbutils
      unstable.pmbootstrap
      # unstable.davinci-resolve
      # unstable.firefox
      # unstable.librewolf
      xcompmgr
      heroic
      killall
      xdo
      # floorp
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
      # TODO FIX
      # (old-23.gimp.override { withPython = true; })
      unstable.gimp3
      krita
      mpv
      libva-utils
      inkscape
      evince
      sidequest
      xorg.xwininfo
      xboxdrv
      # mangohud
      lua
      apktool
      apksigner
      # my.alvr

      jq
      pulseaudio
      gnome-system-monitor
      zenity
      zen-browser
      nix-tree
      gnome-sound-recorder
      # tigervnc
      # x11vnc
      cabal2nix
      qgnomeplatform
      qgnomeplatform-qt6
      dbeaver-bin
      yad
      ccls
      wofi
      # jupyter
      docker-compose
      playerctl
      libusb1
      solaar
      piper
      # rocketchat-desktop
      tetex
      gnumake
      btop
      calibre
      xorg.xdpyinfo
      unstable.postman
      unstable.brave
      peco
      ffmpeg-full
      d-spy
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
      anki-bin
      compfy
      unstable.picom
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
      # legcord
      # unstable.tdesktop
      # (unstable.qt6Packages.callPackage /etc/nixos/pkgs/tdesktop/tdesktop.nix {
      #   abseil-cpp = unstable.abseil-cpp_202111;
      # })
      # unstable.tdesktop
      # telegram-desktop_git
      # (telegram-desktop.overrideAttrs(oldAttrs: rec {
      #   name = "tdesktop";
      #   version = "p5.13.1";
      #   src = fetchFromGitHub {
      #     owner = "p2d0";
      #     repo = "tdesktop";
      #     rev = "a2422d1a49d9ac4cf1387312418e1772efb6195b";
      #     fetchSubmodules = true;
      #     sha256 = "sha256-/x7VZssjPk17gYSXxU/4KdHSdOjxW8ncWxItwj7lA/Y=";};
      # }))
      tdesktop_p2d0
      nil
      unstable.nixd
      jpegoptim
      chatterino2
      filelight
      polkit_gnome
      dconf-editor
      gnome-characters
      minidlna
      gedit
      ntfs3g
      # gnome.gnome-boxes
      # unstable.rustdesk
      my.trex
      unstable.qbittorrent
      # qbittorrent-qt5
      # epiphany
      # tor-browser-bundle-bin
      # looking-glass-client
      # unstable.tg
      # pkgs.cinnamon.nemo-with-extensions
      # pkgs.cinnamon.nemo-fileroller
      # pkgs.cinnamon.nemo-python
      # pkgs.libsForQt5.dolphin
      # unstable.telegram-cli
      sounduxPkgs.soundux
      # my.soundux
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
      nautilus
      nautilus-python
      spice-vdagent
      inetutils
      zip
      xsettingsd
      easyeffects
      # evolution
      nodejs
      iconpack-obsidian
      kdePackages.xwaylandvideobridge
      (pkgs.wrapOBS {
        plugins = with pkgs.unstable.obs-studio-plugins; [
          obs-gstreamer
          obs-vkcapture
          obs-vaapi
          wlrobs
          droidcam-obs
        ];
      })
      # libreoffice
      pkgs.onlyoffice-bin
      koreader
      stremio
      # vlc
      gsettings-desktop-schemas
      wineWowPackages.stable
      # whatsapp-for-linux
      libvirt
      dunst
      android-tools
      # sublime
      # drawio
      pipenv
      unstable.spotify
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
      # unstable.natron
      unstable.scrcpy
      unstable.yt-dlp
      # pythonPackages.yt-dlp
      imagemagick
      # thunderbird
      # libpulseaudio
      # pythonPackages.virtualenv
      # pythonPackages.pip
      unstable.anydesk
      feh
      eog
      alacritty
      dmenu
      gnome-disk-utility
      cabal2nix
      htop
      unzip
      audacity
      my.get_current_screen_geometry
      # (pkgs.callPackage /etc/nixos/pkgs/get_current_screen_geometry.nix { })
      # NOTE https://nixos.wiki/wiki/Nixpkgs/Modifying_Packages
      # guake
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
