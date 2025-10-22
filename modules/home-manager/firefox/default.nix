{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.firefox;
in {
  options.modules.firefox = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    # programs.firefox = {
    #   enable = true;
    #   profiles =
    #     let
    #       userChromeContent = lib.concatStrings [
    #         "/* Generated userChrome.css */\n\n"
    #         (builtins.readFile ./autohide_toolbox.css)
    #         "\n/* autohide_main_toolbar.css commented out */\n\n"
    #         (builtins.readFile ./hide_tabs_toolbar_v2.css)
    #         "\n"
    #         (builtins.readFile ./autohide_sidebar.css)
    #         "\n"
    #         (builtins.readFile ./sidebery.css)
    #         "\n/* compact_urlbar_megabar.css commented out */\n\n"
    #         "#sidebar-header {\n    display: none;\n}\n"
    #         "#sidebar{\n    border:none;\n    padding-left: 5px;\n}\n"
    #       ]; in {
    #         default = {
    #           id = 0;
    #           name = "default";
    #           isDefault = true;
    #           userChrome = userChromeContent;
    #         };
    #       };
    # };
  };
}
