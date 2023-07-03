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
            fonts.fontconfig = { enable = true; };
            fonts.fontconfig.defaultFonts = {
                  monospace = [ "Noto Sans Mono" ];
                  sansSerif = [ "Noto Sans" ];
                  serif = [ "Noto Sans" ];
            };
            fonts.fontDir.enable = true;
            fonts.fonts = with pkgs; [
                  pkgs.jetbrains-mono
                  pkgs.font-awesome
                  pkgs.freefont_ttf
                  # pkgs.nerdfonts
                  (pkgs.nerdfonts.override {
                    fonts = [ "FiraCode" "DroidSansMono" "ShareTechMono" "Noto" "JetBrainsMono" ];
                  })
                  pkgs.weather-icons
                  (callPackage ./fonts/bellandlamb.nix { })
                  (callPackage ./fonts/apex.nix { })
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
                  pkgs.noto-fonts-emoji
                  pkgs.roboto
                  pkgs.noto-fonts
                  pkgs.noto-fonts-extra
                  pkgs.fira-code
                  pkgs.hanazono
                  pkgs.dejavu_fonts
                  pkgs.material-design-icons
                  pkgs.material-icons
            ];
      };
}
