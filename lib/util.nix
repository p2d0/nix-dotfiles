{ lib, ... }:

rec {
  allUsers = items: {
    home-manager.users."andrew" = items;
    home-manager.users."andrew-work" = items;
  };
  withHomeVars = homeVars: items: items //
                    (if builtins.typeOf homeVars == "lambda"
                     then {
                       home-manager.users."andrew" = homeVars;
                       home-manager.users."andrew-work" = homeVars;
                     }
                     else {
                       home-manager.users."andrew" = ({...}:homeVars);
                       home-manager.users."andrew-work" = ({...}:homeVars);
                     } )
  ;

  # mapFilterAttrs ::
  #   (name -> value -> bool)
  #   (name -> value -> { name = any; value = any; })
  #   attrs
  mapFilterAttrs = pred: f: attrs: lib.filterAttrs pred (lib.mapAttrs' f attrs);

  mapModules = dir: fn:
    mapFilterAttrs
      (n: v:
        v != null &&
        !(lib.hasPrefix "_" n))
      (n: v:
        let path = "${toString dir}/${n}"; in
        if v == "directory" && lib.pathExists "${path}/default.nix"
        then lib.nameValuePair n (fn path)
        else if v == "regular" &&
                n != "default.nix" &&
                lib.hasSuffix ".nix" n
        then lib.nameValuePair (lib.removeSuffix ".nix" n) (fn path)
        else lib.nameValuePair "" null)
      (builtins.readDir dir);
}
