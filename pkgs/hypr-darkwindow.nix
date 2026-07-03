{
  config,
  lib,
  callPackage,
  fetchFromGitHub,
  nix-update-script,
  pkg-config,
  hyprland,
}@topLevelArgs:

let
  mkHyprlandPlugin = lib.extendMkDerivation {
    constructDrv = topLevelArgs.hyprland.stdenv.mkDerivation;

    extendDrvArgs =
      finalAttrs:
      {
        pluginName ? "",
        nativeBuildInputs ? [ ],
        buildInputs ? [ ],
        hyprland ? topLevelArgs.hyprland,
        ...
      }@args:

      {
        pname = "${pluginName}";
        nativeBuildInputs = [ pkg-config ] ++ nativeBuildInputs;
        buildInputs = [ hyprland ] ++ hyprland.buildInputs ++ buildInputs;
        meta = args.meta // {
          description = args.meta.description or "";
          longDescription =
            (args.meta.longDescription or "")
            + "\n\nPlugins can be installed via a plugin entry in the Hyprland NixOS or Home Manager options.";

          platforms = args.meta.platforms or hyprland.meta.platforms or [ ];
        };
      };
  };
in
mkHyprlandPlugin (finalAttrs: {
  pluginName = "hypr-darkwindow";
  version = "0.55.0";

  src = fetchFromGitHub {
    owner = "micha4w";
    repo = "Hypr-DarkWindow";
    tag = "v${finalAttrs.version}";
    hash = "sha256-UQEPjJu4kYBzwMp8SPf+O48l3UyPDFmOjKDKh+X273I=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib
    mv out/hypr-darkwindow.so $out/lib/libhypr-darkwindow.so

    runHook postInstall
  '';

  passthru.updateScript = nix-update-script { };

  meta = {
    description = "Hyprland Plugin to invert Colors of specific Windows!";
    homepage = "https://github.com/micha4w/Hypr-DarkWindow";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ anninzy ];
    platforms = lib.platforms.linux;
  };
})
