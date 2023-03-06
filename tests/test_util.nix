{ pkgs, lib }:

let my = import ../lib/util.nix { inherit lib; };
in
lib.runTests {
  xtestEasy = {
    expr = (my.allUsers ({}: {
      home.file = {
        ".xmonad/lib".source = "pepega";

        ".xmonad/xmonad.hs" = {
          source = "pepega";
        };
      };
    }));
    expected = { kek = "y";};
  };
  testMapModules = {
    expr  = (my.mapModules /etc/nixos/pkgs (p: lib.id p));
    expected = [];
  };
}
