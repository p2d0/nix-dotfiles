let
  pkgs = import /mnt/md127/nixpkgs {};
  foo = pkgs.gcc;
in
with pkgs; [
  htop hello

]
