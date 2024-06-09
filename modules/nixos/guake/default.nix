{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.guake;
in {
  options.modules.guake = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable (lib.my.withHome
    {
      dconf = {
        enable = true;
        settings = {
          "general" = {
            abbreviate-tab-names = false;
            compat-delete = "delete-sequence";
            display-n = 0;
            display-tab-names = 0;
            gtk-prefer-dark-theme = true;
            gtk-theme-name = "Adwaita";
            gtk-use-system-default-theme = true;
            hide-tabs-if-one-tab = false;
            history-size = 1000;
            load-guake-yml = true;
            max-tab-name-length = 100;
            mouse-display = true;
            open-tab-cwd = true;
            prompt-on-quit = true;
            quick-open-command-line = "gedit %(file_path)s";
            restore-tabs-notify = true;
            restore-tabs-startup = false;
            save-tabs-when-changed = true;
            schema-version = "3.9.0";
            scroll-keystroke = true;
            start-at-login = true;
            use-default-font = true;
            use-popup = true;
            use-scrollbar = true;
            use-trayicon = true;
            window-halignment = 0;
            window-height = 40;
            window-losefocus = false;
            window-ontop = true;
            window-refocus = false;
            window-tabbar = true;
            window-vertical-displacement = 0;
            window-width = 100;
          };

          "keybindings/global" = {
            show-hide = "F12";
          };

          "keybindings/local" = {
            close-tab = "<Primary>w";
            move-tab-left = "<Primary><Alt>h";
            move-tab-right = "<Primary><Alt>l";
            new-tab = "<Primary>t";
            next-tab = "<Alt>l";
            previous-tab = "<Alt>h";
          };

          "style/background" = {
            transparency = 90;
          };

          "style/font" = {
            allow-bold = true;
            bold-is-bright = false;
            palette = "#000000000000:#cccc00000000:#4e4e9a9a0606:#c4c4a0a00000:#34346565a4a4:#757550507b7b:#060698209a9a:#d3d3d7d7cfcf:#555557575353:#efef29292929:#8a8ae2e23434:#fcfce9e94f4f:#72729f9fcfcf:#adad7f7fa8a8:#3434e2e2e2e2:#eeeeeeeeecec:#ffffffffffff:#000000000000";
            palette-name = "Tango";
            style = "Noto Sans Mono 10";
          };

        };
      };
    }
    {
      programs.dconf.enable = true;
      environment.systemPackages = [
        pkgs.guake
      ];
    });
}
