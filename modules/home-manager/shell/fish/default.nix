{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.shell.fish;
in {
  options.modules.shell.fish = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
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
      {
        name = "fzf";
        src = pkgs.fetchFromGitHub {
          owner = "PatrickF1";
          repo = "fzf.fish";
          rev = "master";
          sha256 = "sha256-T8KYLA/r/gOKvAivKRoeqIwE2pINlxFQtZJHpOy9GMM=";
        };
      }
    ];
    programs.fish.shellInit = ''
        fish_vi_key_bindings
        fish_add_path  $HOME/config/emacs/bin
        fish_add_path  $HOME/.npm-packages/bin
        fish_add_path  $HOME/.npm-packages/lib/node_modules
        alias s="sgpt -se"
        alias nv="neovide --multigrid --frame none --maximized"
        if status is-interactive
        and not set -q TMUX
            exec tmux
        end
        function fish_user_key_bindings
            fzf_configure_bindings --directory=\cf --variables=\e\cv --git_log=\cl
            bind -M normal -m insert \cr 'peco_select_history (commandline -b)'
            bind -M insert \cr 'peco_select_history (commandline -b)'

        end
'';
    home.file = {
      # ".config/fish/conf.d".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/home-manager/shell/fish/conf.d;
      ".config/fish/conf.d" = {
        source = ./conf.d;
        recursive = true;
      };
      ".config/fish/functions".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/home-manager/shell/fish/functions;
      ".config/fish/completions".source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/modules/home-manager/shell/fish/completions;
    };

  };
}
