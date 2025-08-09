{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  fetchNpmDeps,
  nix-update-script,
}:
buildNpmPackage (finalAttrs: {
  pname = "qwen-code";
  version = "v0.0.5-nightly.7";
  src = fetchFromGitHub {
    owner = "QwenLM";
    repo = "qwen-code";
    rev = "9875ea6479da3a21e9e48636b9eda2ef6453a243";
    hash = "sha256-A/f4MDHICxwXj+OqG+QQPwnPhNmE3NbZxmgeVYKIiJE=";
  };

  npmDeps = fetchNpmDeps {
    inherit (finalAttrs) src;
    hash = "sha256-O9STim5g6FlbZn03XQ7C25tdKMksLpUdjLg1C5kmEqY=";
  };
  buildPhase = ''
    runHook preBuild
    npm run generate
    npm run bundle
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp -r bundle/* $out/
    substituteInPlace $out/gemini.js --replace '/usr/bin/env node' "$(type -p node)"
    ln -s $out/gemini.js $out/bin/qwen-code
    runHook postInstall
  '';
  passthru.updateScript = nix-update-script { };
  meta = {
    description = "Qwen-code is a coding agent that lives in digital world";
    homepage = "https://github.com/QwenLM/qwen-code";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ ];
    mainProgram = "qwen-code";
    platforms = lib.platforms.all;
  };
})
