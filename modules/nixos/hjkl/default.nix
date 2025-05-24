{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.hjkl;
    xkeyboard = pkgs.xorg.xkeyboardconfig_custom {
      layouts = config.services.xserver.xkb.extraLayouts;
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
    services.xserver.xkb.extraLayouts.us-hjkl = {
      description = "US layout with hjkl";
      languages = [ "eng" ];
      symbolsFile = pkgs.writeText "us-hjkl" ''
partial keypad_keys
xkb_symbols "us-hjkl"
{
    key <CAPS> { [ ISO_Level3_Shift                     ] };
    key <AD02> { [ w, W, U2191, U2191                   ] };
    key <AC01> { [ a, A, U2190, U2190                   ] };
    key <AC02> { [ s, S, U2193, U2193                   ] };
    key <AC03> { [ d, D, U2192, U2192                   ] };
    key <AB02> { [ x, X, Delete, Delete                 ] };
    key <AC06> { [ h, H, Left, Left                     ] };
    key <AC07> { [ j, J, Down, Down                     ] };
    key <AC08> { [ k, K, Up, Up                         ] };
    key <AC09> { [ l, L, Right, Right                   ] };
    key <AD07> { [ u, U, Prior, Prior                   ] };
    key <AE03> { [ 3, section, Home, Home               ] };
    key <AE04> { [ 4, dollar, End, End                  ] };
    key <AD09> { [ o, O, Page_Up, Page_Up               ] };
    key <AD10> { [ p, P, Page_Down, Page_Down           ] };
};
'';
    };
    # key <AC05> { [ g, G, F12, F12                      ] };
    #
   # key <AC04> { [ s, S, U2193, U2193                   ] };
   # key <AC05> { [ d, D, U2192, U2192                   ] };
    # services.xserver.layout = mkOverride 1 "us";
    # TODO fix
    environment.sessionVariables = {
      XKB_CONFIG_ROOT = mkOverride 1 "${xkbrdz}/etc/X11/xkb";
    };
    services.xserver.xkb.dir = mkOverride 1 "${xkbrdz}/etc/X11/xkb";
    services.xserver = {
      exportConfiguration = mkOverride 1 true;
    };
  };
}
