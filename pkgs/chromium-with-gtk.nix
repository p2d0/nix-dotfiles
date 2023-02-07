{ pkgs ? import <nixpkgs> {}}:

pkgs.chromium.mkDerivation (base: rec {
  name = "dark-mode-chromium";
  gnFlags = { test_flag = 42; };
  # patches = [
  #   (pkgs.fetchpatch {
  #     url = "https://raw.githubusercontent.com/PF4Public/gentoo-overlay/84f77f55a09f6ce0523591d7c300bf9deb2a1c01/www-client/ungoogled-chromium/files/gtk-fix-prefers-color-scheme-query.diff";
  #     sha256 = "sha256-y0v+ZArCsKVKGYSBChpugvPUmXiwnAxYHkVhAgT/aOk=";
  #   })];
})
