{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.pi;

  settingsJson = lib.generators.toJSON {} {
    lastChangelogVersion = cfg.package.version;
    defaultProvider = cfg.defaultProvider;
    defaultModel = cfg.defaultModel;
    defaultThinkingLevel = cfg.defaultThinkingLevel;
    compaction = cfg.compaction;
    packages = cfg.extensions;
    hideThinkingBlock = cfg.hideThinkingBlock;
  };
in
{
  options.modules.pi = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
    package = mkOption {
      type = types.package;
      default = pkgs.my.pi-coding-agent;
    };
    defaultProvider = mkOption {
      type = types.str;
      default = "";
      description = "Default provider for pi";
    };
    defaultModel = mkOption {
      type = types.str;
      default = "";
      description = "Default model for pi";
    };
    defaultThinkingLevel = mkOption {
      type = types.str;
      default = "medium";
      description = "Default thinking level (none/low/medium/high/auto)";
    };
    compaction = mkOption {
      type = types.submodule {
        options = {
          enabled = mkOption {
            type = types.bool;
            default = true;
          };
          reserveTokens = mkOption {
            type = types.int;
            default = 8192;
          };
          keepRecentTokens = mkOption {
            type = types.int;
            default = 8192;
          };
        };
      };
      default = { };
      description = "Compaction settings for pi";
    };
    extensions = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "List of npm packages for pi (populates settings.json.packages)";
      example = [ "npm:pi-caveman" "npm:pi-lens" ];
    };
    hideThinkingBlock = mkOption {
      type = types.bool;
      default = false;
      description = "Hide thinking blocks in output";
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];

    home.file.".pi/agent" = {
      source = config.lib.file.mkOutOfStoreSymlink /etc/nixos/configs/pi;
    };

    home.file.".pi/settings.json" = {
      text = settingsJson;
    };
  };
}
