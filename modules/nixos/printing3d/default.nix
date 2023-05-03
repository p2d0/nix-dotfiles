{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.printing3d;
in {
  options.modules.printing3d = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs;[
      blender
      cura
      # Wrapping without rebuilding
      # https://stackoverflow.com/questions/68523367/in-nixpkgs-how-do-i-override-files-of-a-package-without-recompilation/68523368#68523368
      # https://discourse.nixos.org/t/overriding-a-package-without-rebuilding-it/13898/6

      freecad

      # (pkgs.symlinkJoin {
      #   name = "freecad-wrapped";
      #   paths = [ pkgs.freecad ];
      #   nativeBuildInputs = [ pkgs.makeBinaryWrapper ];
      #   buildInputs = [pkgs.gmsh];
      #   runtimeDependencies = [pkgs.gmsh];
      #   postBuild = ''
      #   wrapProgram $out/bin/freecad \
      #               --prefix PATH : ${lib.makeBinPath [pkgs.gmsh]}
      #   '';


      # })

      # (freecad.overrideAttrs(oldAttrs: rec {
      #     qtWrapperArgs = oldAttrs.qtWrapperArgs ++ [
      #       "--prefix PATH : ${pkgs.gmsh}/bin"
      #     ];
      #   }))
    ];
  };
}
