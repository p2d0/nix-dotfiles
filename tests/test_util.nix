{ pkgs, lib }:

let my = import ../lib/util.nix { inherit lib; };
in
lib.runTests {
  testEasy =
    let result = (my.allUsers ({}: {
          home.file = {
            ".xmonad/lib".source = "pepega";

            ".xmonad/xmonad.hs" = {
              source = "pepega";
            };
          };
        }));
    in {
      expr = lib.hasAttr "home-manager" result;
      expected = true;
    };
  testModified =
    let result = (my.withHome
      ({...}:{
        home.file = {
          ".xmonad/lib".source = "pepega";

          ".xmonad/xmonad.hs" = {
            source = "pepega";
          };
        };})
      {
        kek = "";
      });
    in {
      expr = lib.hasAttr "home-manager" result && lib.hasAttr "kek" result;
      expected = true;
    };
  testJustAttrSet =
    let result = (my.withHome
      {
        home.file = {
          ".xmonad/lib".source = "pepega";

          ".xmonad/xmonad.hs" = {
            source = "pepega";
          };
        };
      }
      {
        kek = "";
      } );
    in {
      expr = (builtins.typeOf result.home-manager.users.andrew) == "lambda";
      # lib.hasAttr "home-manager" result && lib.hasAttr "kek" result;
      expected = true;
    };


  xtestMapModules = {
    expr  = (my.mapModules /etc/nixos/pkgs (p: lib.id p));
    expected = [];
  };
}
