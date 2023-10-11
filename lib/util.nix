{ lib, ... }:

with lib;
let
  inherit (lib) mkOption types;
in
rec {
  allUsers = items: {
    home-manager.users."andrew" = items;
    # home-manager.users."andrew-work" = items;
  };
  withHome = homeVars: items: items //
                              (if builtins.typeOf homeVars == "lambda"
                               then {
                                 home-manager.users."andrew" = homeVars;
                                 # home-manager.users."andrew-work" = homeVars;
                               }
                               else {
                                 home-manager.users."andrew" = ({...}:homeVars);
                                 # home-manager.users."andrew-work" = ({...}:homeVars);
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

  mapTests = dir: fn:
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
                lib.hasInfix "test" (builtins.baseNameOf n) &&
                lib.hasSuffix ".nix" n
        then lib.nameValuePair (lib.removeSuffix ".nix" n) (fn path)
        else lib.nameValuePair "" null)
      (builtins.readDir dir);

  mapModulesRec = dir: fn:
    mapFilterAttrs
      (n: v:
        v != null &&
        !(hasPrefix "_" n))
      (n: v:
        let path = "${toString dir}/${n}"; in
        if v == "directory" && !(lib.pathExists "${path}/default.nix")
        then nameValuePair n (mapModulesRec path fn)
        else if v == "directory"
        then nameValuePair n (fn path)
        else nameValuePair "" null)
      (builtins.readDir dir);

  mapModules' = dir: fn:
    attrValues (mapModules dir fn);

  # mapModulesRec' = dir: fn:
  #   let
  #     dirs =
  #       mapAttrsToList
  #         (traceVal (k: _: (traceVal "${dir}/${k}")) )
  #         (traceVal (filterAttrs
  #           (n: v: builtins.trace ''Directory ${dir},Name ${n};Value ${v}'' (v == "directory"  && !(hasPrefix "_" n)))
  #           (traceVal (builtins.readDir dir) )) );
  #     files = attrValues (traceVal (mapModules dir id) );
  #     paths = (traceVal files) ++ concatLists (traceVal (map (d: (traceVal (mapModulesRec' (traceVal d) id ))) dirs) );
  #   in map fn (traceVal paths);

  mapModulesRec' = dir: fn:
    flatten (builtins.map (s: (if builtins.typeOf s == "set" then (attrValues s) else s))
      (attrValues (mapModulesRec dir fn)));

  findAllModulePathsIn = dir:
    mapModulesRec' dir id;

  arrModulesRec = dir: fn:
    mapFilterAttrs
      (n: v:
        v != null &&
        !(hasPrefix "_" n))
      (n: v:
        let path = "${toString dir}/${n}"; in
        if v == "directory" && !(lib.pathExists "${path}/default.nix")
        then nameValuePair n (mapModulesRec path fn)
        else if v == "directory"
        then nameValuePair n (fn path)
        else nameValuePair "" null)
      (builtins.readDir dir);

  mkOpt  = type: default:
    mkOption { inherit type default; };

  mkOpt' = type: default: description:
    mkOption { inherit type default description; };

  mkBoolOpt = default: mkOption {
    inherit default;
    type = types.bool;
    example = true;
  };
}
