{ config, lib, pkgs, ... }:

let
  get_ssh_file = x: {source = /mnt/md127/backup_arch/.ssh/${x};};
in
{
  home.file = {
    ".ssh/id_rsa" = get_ssh_file "id_rsa";
    ".ssh/id_rsa.pub" = get_ssh_file "id_rsa.pub";
    ".ssh/id_ed25519" = get_ssh_file "id_ed25519";
    ".ssh/id_ed25519.pub" = get_ssh_file "id_ed25519.pub";
    ".ssh/config" =  {
      text = ''dummy'';
      target = "/mnt/md127/backup_arch/.ssh/config";
      onChange = ''cat /mnt/md127/backup_arch/.ssh/config > ~/.ssh/config && chmod 400 ~/.ssh/config'';
    };
  };
}
