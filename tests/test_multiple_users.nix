{ lib }:

let my = import ../lib/multiuser.nix { };
in
lib.runTests {
  testEasy = {
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
}
