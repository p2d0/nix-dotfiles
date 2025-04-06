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
      (let cura5 = appimageTools.wrapType2 rec {
             name = "cura5";
             version = "5.4.0";
             src = fetchurl {
               url = "https://github.com/Ultimaker/Cura/releases/download/${version}/UltiMaker-Cura-${version}-linux-modern.AppImage";
               hash = "sha256-QVv7Wkfo082PH6n6rpsB79st2xK2+Np9ivBg/PYZd74=";
             };
           }; in writeScriptBin "cura" ''
      #! ${pkgs.bash}/bin/bash
      # AppImage version of Cura loses current working directory and treats all paths relateive to $HOME.
      # So we convert each of the files passed as argument to an absolute path.
      # This fixes use cases like `cd /path/to/my/files; cura mymodel.stl anothermodel.stl`.
      args=()
      for a in "$@"; do
        if [ -e "$a" ]; then
          a="$(realpath "$a")"
        fi
        args+=("$a")
      done
      exec "${cura5}/bin/cura5" "''${args[@]}"
    '')
      # Wrapping without rebuilding
      # https://stackoverflow.com/questions/68523367/in-nixpkgs-how-do-i-override-files-of-a-package-without-recompilation/68523368#68523368
      # https://discourse.nixos.org/t/overriding-a-package-without-rebuilding-it/13898/6

      # freecad
      openscad

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
