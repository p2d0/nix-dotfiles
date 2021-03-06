# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ghcWithPackages, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      <home-manager/nixos>
    ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/vda"; # or "nodev" for efi only

  networking.hostName = "andrew"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Moscow";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp1s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
	  font = "Lat2-Terminus16";
	  keyMap = "us";
  };

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    displayManager.defaultSession = "none+xmonad";
  };

  

  # Configure keymap in X11
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.andrew = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
nixpkgs.overlays = [
(self: super: {
haskellPackages = super.haskellPackages.override {
overrides = hself: hsuper: {
xmonad = hsuper.xmonad_0_17_0;
xmonad-contrib = hsuper.xmonad-contrib_0_17_0;
xmonad-extras = hsuper.xmonad-extras_0_17_0;
};
};
})
];
nixpkgs.config.allowUnfree = true;
  home-manager.users.andrew = {pkgs, callPackage, ...}: {
	  nixpkgs.config.allowUnfree = true;
	  home.packages = [
		  pkgs.pasystray
			  pkgs.redshift
			  pkgs.dropbox
			  pkgs.dunst
			  pkgs.tdesktop
			  pkgs.rofi
				pkgs.firefox
      pkgs.alacritty
      pkgs.dmenu
      pkgs.htop
      (pkgs.haskellPackages.ghcWithPackages (self:
        [
          self.xmonad
          self.xmonad-contrib
          self.xmonad-extras
        ]
      ))
    ];


	  services.emacs.package = pkgs.emacsUnstable;
	  nixpkgs.overlays = [
		  (import
		   (builtins.fetchTarball {
		    url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
		    }))
      (self: super: {
        haskellPackages = super.haskellPackages.override {
          overrides = hself: hsuper: {
            xmonad = hsuper.xmonad_0_17_0;
            # xmonad-with-packages = hsuper.xmonad-with-packages_0_17_0;
            xmonad-contrib = hsuper.xmonad-contrib_0_17_0;
            xmonad-extras = hsuper.xmonad-extras_0_17_0;
          };
        };
      })
	  ];
	  services.emacs.enable = true;
	  programs.git = {
		  enable = true;
		  userName = "patriot720";
		  userEmail = "cerkin-3@yandex.ru";
	  };

    home.file = {
      ".xmonad" = {
         source = ./xmonad;
         recursive = true;
      };
      ".config/rofi" = {
        source = ./rofi;
        recursive = true;
      };
    };
    xsession = {
      enable = true;
      initExtra = ''
      feh --bg-fill /etc/nixos/bg.jpg
      '';
      windowManager.xmonad =
        {
          enable = true;
          enableContribAndExtras = true;
      };

    };
  };

services = {
	#gnome.gnome-keyring.enable = true;
	#dbus.enable = true;
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    emacs
    tmux
    git
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

