{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
  fetchNpmDeps,
  nix-update-script,
}:
buildNpmPackage (finalAttrs: {
  pname = "qwen-code";
  version = "v0.0.10-nightly.2";
  src = fetchFromGitHub {
    owner = "QwenLM";
    repo = "qwen-code";
    rev = "50b94383def83d4621faf03906eafcdfe074ca44";
    hash = "sha256-PoeSpid7WJwHS2RRd09FOaEvnJw4qJKLGijNeMc68xE=";
  };

  npmDeps = fetchNpmDeps {
    inherit (finalAttrs) src;
    hash = "sha256-4MNz8BmRG2hFbzlsoy7kEBr0l2S5cs/ZmWcHTxTvryc=";
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
