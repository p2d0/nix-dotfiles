{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.fonts;
in {
      options.modules.fonts = {
            enable = mkOption {
                  type = types.bool;
                  default = false;
                  example = "";
                  description = ''
      '';
            };
      };
      config = mkIf cfg.enable {
            fonts.fontconfig = {
                  enable = true;
                  allowBitmaps = true;
                  useEmbeddedBitmaps = true;
            };
            fonts.fontconfig.defaultFonts = {
                  monospace = [ "Noto Sans Mono" ];
                  sansSerif = [ "Noto Sans" ];
                  serif = [ "Noto Sans" ];
            };
            fonts.fontDir.enable = true;
            fonts.packages = with pkgs; [
                  pkgs.jetbrains-mono
                  pkgs.font-awesome
                  pkgs.ibm-plex
                  pkgs.freefont_ttf
                  pkgs.nerd-fonts.fira-code
                  pkgs.nerd-fonts.droid-sans-mono
                  pkgs.nerd-fonts.shure-tech-mono
                  pkgs.nerd-fonts.noto
                  pkgs.nerd-fonts.jetbrains-mono
                  pkgs.weather-icons
                  pkgs.texlivePackages.alfaslabone
                  (callPackage ./fonts/bellandlamb.nix { })
                  (callPackage ./fonts/sfpro.nix { })
                  (callPackage ./fonts/icomoon.nix { })
                  (callPackage ./fonts/apex.nix { })
                  (callPackage ./fonts/cryptocoins.nix { })
                  #corduoy https://www.behance.net/gallery/10761523/Corduroy-Slab-Free
                  # hamster script https://www.behance.net/gallery/24882765/Hamster-Script-(Free-Font)
                  # https://www.behance.net/gallery/77444055/APEX-MK3-FREE-ROBUST-DISPLAY-TYPEFACE
                  # FONTS https://visualcomposer.com/blog/free-fonts-for-commercial-use-to-download-in-2022/
                  pkgs.fantasque-sans-mono
                  pkgs.comfortaa
                  pkgs.arphic-uming
                  pkgs.source-han-code-jp
                  pkgs.baekmuk-ttf
                  pkgs.ipafont
                  pkgs.noto-fonts-cjk-sans
                  unstable.noto-fonts-color-emoji
                  # pkgs.noto-fonts-emoji
                  #                   (unstable.noto-fonts-color-emoji.overrideAttrs(oldAttrs: rec {
                  #                         installPhase = ''
                  #         runHook preInstall
                  #         mkdir -p $out/share/fonts/noto
                  #         cp NotoColorEmoji.ttf $out/share/fonts/noto
                  #         cp NotoColorEmoji-emojicompat.ttf $out/share/fonts/noto
                  #         runHook postInstall
                  # '';
                  #                   }))

                  pkgs.roboto
                  pkgs.noto-fonts
                  # pkgs.noto-fonts-extra
                  pkgs.fira-code
                  pkgs.hanazono
                  pkgs.dejavu_fonts
                  pkgs.material-symbols
                  # pkgs.material-design-icons
                  # pkgs.material-icons
            ];
      };
}
