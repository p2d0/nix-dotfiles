{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.fish;
in {
  options.modules.fish = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    users.defaultUserShell = pkgs.fish;
    programs.fish.enable = true;
    programs.fish.plugins = [
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "ddeb28a7b6a1f0ec6dae40c636e5ca4908ad160a";
          sha256 = "0c5i7sdrsp0q3vbziqzdyqn4fmp235ax4mn4zslrswvn8g3fvdyh";
        };
      }
      {
        name = "peco";
        src = pkgs.fetchFromGitHub {
          owner = "oh-my-fish";
          repo = "plugin-peco";
          rev = "master";
          sha256 = "sha256-EUoicPd+aUMlfCeo9BOuIiBlQSpPtMtMn5AUkZU3uQA=";
        };
      }
    ];
    programs.fish.shellInit = ''
        fish_vi_key_bindings
        fish_add_path  $HOME/.emacs.d/bin
        fish_add_path  $HOME/.npm-packages/bin
        fish_add_path  $HOME/.npm-packages/lib/node_modules
        alias s="sgpt -se"
        alias nv="neovide --multigrid --frame none --maximized"
        function fish_user_key_bindings
            bind -M normal -m insert \cr 'peco_select_history (commandline -b)'
            bind -M insert \cr 'peco_select_history (commandline -b)'
        end'';
    home.file = {
      ".config/fish/conf.d" = {
        source = ./conf.d;
        recursive = true;
      };
      ".config/fish/functions".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/fish/functions;
      ".config/fish/completions".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/fish/completions;
    };

  };
}
