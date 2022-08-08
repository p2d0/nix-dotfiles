{stdenv,pkgs, lib}:

with pkgs;
pkgs.ubootTools.overrideAttrs (oldAttrs: rec {
  filesToInstall = oldAttrs.filesToInstall ++ [" tools/mksunxiboot"];
  installPhase = "runHook preInstall\n\nmkdir -p $out/bin\ncp tools/dumpimage tools/mksunxiboot tools/fdtgrep tools/kwboot tools/mkenvimage tools/mkimage $out/bin\n\nmkdir -p \"$out/nix-support\"\necho \"file binary-dist $out/bin/dumpimage\" >> \"$out/nix-support/hydra-build-products\"\necho \"file binary-dist $out/bin/fdtgrep\" >> \"$out/nix-support/hydra-build-products\"\necho \"file binary-dist $out/bin/kwboot\" >> \"$out/nix-support/hydra-build-products\"\necho \"file binary-dist $out/bin/mkenvimage\" >> \"$out/nix-support/hydra-build-products\"\necho \"file binary-dist $out/bin/mkimage\" >> \"$out/nix-support/hydra-build-products\"\n\n\nrunHook postInstall\n";
})
