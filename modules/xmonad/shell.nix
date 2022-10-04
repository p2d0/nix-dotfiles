{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.hello
    (pkgs.haskellPackages.ghcWithPackages (self: [
      self.haskell-language-server # TODO not system wide?
      self.xmonad
      self.xmonad-contrib
      self.xmonad-extras
    ]))
    # keep this line if you use bash
    # pkgs.bashInteractive
  ];
}
