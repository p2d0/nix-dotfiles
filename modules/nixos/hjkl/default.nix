{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.hjkl;
    xkeyboard = pkgs.xorg.xkeyboardconfig_custom {
      layouts = config.services.xserver.extraLayouts;
    };
    xkbrdz = xkeyboard.overrideAttrs(oldAttrs: {
      postPatch = ''sed -i '0,/{/ {s/{/&\n    include "us-hjkl(us-hjkl)"/}' symbols/us;
                  '' + oldAttrs.postPatch;
    });
in {
  options.modules.hjkl = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };
  config = mkIf cfg.enable {
    services.xserver.extraLayouts.us-hjkl = {
      description = "US layout with hjkl";
      languages = [ "eng" ];
      symbolsFile = pkgs.writeText "us-hjkl" ''
      partial keypad_keys
      xkb_symbols "us-hjkl"
      {
      key <CAPS> { [ ISO_Level3_Shift            ] };
      key <AB02> { [ x, X, Delete, Delete        ] };
      key <AC02> { [ s, S, KP_Enter, KP_Enter    ] };
      key <AC03> { [ d, D, Next, Next            ] };
      key <AC04> { [ f, F, BackSpace, BackSpace  ] };
      key <AC06> { [ h, H , Left, Left           ] };
      key <AC07> { [ j, J, Down, Down            ] };
      key <AC08> { [ k, K, Up, Up                ] };
      key <AC09> { [ l, L, Right, Right          ] };
      key <AD07> { [ u, U, Prior, Prior          ] };
      key <AE03> { [ 3, section, Home, Home      ] };
      key <AE04> { [ 4, dollar, End, End         ] };
      };'';
    };
    # services.xserver.layout = mkOverride 1 "us";
    # TODO fix
    environment.sessionVariables = {
      XKB_CONFIG_ROOT = mkOverride 1 "${xkbrdz}/etc/X11/xkb";
    };

    services.xserver = {
      xkbDir = mkOverride 1 "${xkbrdz}/etc/X11/xkb";
      exportConfiguration = mkOverride 1 true;
    };
  };
}
