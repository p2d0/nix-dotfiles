{ pkgs, lib }:

let my = import ../lib/util.nix { inherit lib; };
in
lib.runTests {
  xtestEasy =
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
  xtestModified =
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
  xtestJustAttrSet =
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


  xtestMapModulesPkgs = {
    expr  = (my.mapModules /etc/nixos/pkgs (p: lib.id p));
    expected = [];
  };

  xtestMapModulesRec = {
    expr  = (my.mapModulesRec /etc/nixos/modules/home-manager (p: lib.id p));
    expected = [];
  };

  xtestMapModules' = {
    expr  = (my.mapModulesRec' /etc/nixos/modules/home-manager (p: lib.id p));
    expected = [];
  };
  testMapModulesNixos' = {
    expr  = (["kek"]  ++ (my.mapModulesRec' /etc/nixos/modules/nixos (p: lib.id p)) );
    expected = [];
  };

}
