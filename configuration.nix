# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./modules/hjkl.nix
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

  hardware.opengl.enable = true;

  # boot.plymouth.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  boot.loader.grub.enable = true;
  boot.loader.grub.default = "saved";
  boot.loader.grub.version = 2;

  boot.loader.grub.device = "/dev/sda";

  networking.hostName = config.user;

  time.timeZone = "Europe/Moscow";

  virtualisation.docker.enable = true;

  i18n.defaultLocale = "en_US.UTF-8";

  services.xserver = {
    enable = true;
    # Doesnt work
    layout = "us,ru";
    xkbOptions = "grp:alt_shift_toggle";

    libinput = {
      enable = true;
      mouse = {
        accelProfile = "flat";
      };
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
    let
      nixpkgs-tars = "https://github.com/NixOS/nixpkgs/archive/";
    in
      {
        allowUnfree = true;
        allowBroken = true;
        packageOverrides = pkgs: {
          pr181605 = import
            (fetchTarball
              "${nixpkgs-tars}7cc979502c3dc5480ef3e4ffe1a05c897084d34b.tar.gz")
            { config = config.nixpkgs.config; };

        };
      };


  services.blueman.enable = true;
  programs.dconf.enable = true;

  services.udev.packages = with pkgs;
    [
      gnome.gnome-settings-daemon
    ];


  services = {
    gnome.gnome-keyring.enable = true;
    gnome.at-spi2-core.enable = true;
    dbus.enable = true;
  };

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
    GTK_DATA_PREFIX = [
      "${config.system.path}"
    ];
  };

  fonts.fontconfig = { enable = true; };

  fonts.fontconfig.defaultFonts = {
    monospace = [ "Noto Sans Mono" ];
    sansSerif = [ "Noto Sans" ];
    serif = [ "Noto Sans" ];
  };

  fonts.fontDir.enable = true;

  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    emacsNativeComp
    pr181605.kdiskmark
    (pkgs.haskellPackages.callPackage ./modules/taffybar.nix { })
    tmux
    git
    ripgrep
    fd
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:
  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.11"; # Did you read the comment?

}
