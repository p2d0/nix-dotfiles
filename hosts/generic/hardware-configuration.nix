{ config, lib, pkgs, modulesPath, ... }:

{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "ehci_pci" "ata_piix" "usb_storage" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "vfio-pci" ];
  boot.kernelParams = [];
  boot.extraModulePackages = [ ];
  boot.loader.timeout = 1;
  # hardware.hidpi.enable = true;

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/66e90d46-d031-4e40-ad33-c3f156316c20";
      options = [ "noatime" ];
      fsType = "ext4";
    };

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = true;
  # networking.interfaces.enp3s0.useDHCP = true;
  networking.enableIPv6 = false;
}
