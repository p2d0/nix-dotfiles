{
  lib,
  stdenv,
  fetchFromGitHub,
  pkg-config,
  meson,
  ninja,
  rustc,
  cargo,
  rustPlatform,
  xdg-desktop-portal,
  cairo,
  pango,
  libgbm,
  libGL,
  libxkbcommon,
  glib,
  pipewire,
  wayland,
  nix-update-script,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "xdg-desktop-portal-luminous";
  version = "0.1.14";

  src = fetchFromGitHub {
    owner = "waycrate";
    repo = "xdg-desktop-portal-luminous";
    rev = "master";
    hash = "sha256-fXbpQ2a8nEOGcp0r3hwnulPG7yE6ZSVBgVz6YoOEBZA=";
  };

  cargoDeps = rustPlatform.fetchCargoVendor {
    inherit (finalAttrs) pname version src;
    hash = "";
  };

  nativeBuildInputs = [
    pkg-config
    meson
    ninja
    rustc
    cargo
    rustPlatform.cargoSetupHook
    rustPlatform.bindgenHook
  ];

  buildInputs = [
    xdg-desktop-portal
    cairo
    pango
    glib
    pipewire
    libgbm
    libGL
    libxkbcommon
  ];

  postInstall = ''
    patchelf \
      --add-needed libwayland-client.so.0 \
      --add-rpath ${lib.makeLibraryPath [ wayland ]} \
      $out/libexec/xdg-desktop-portal-luminous
  '';

  passthru.updateScript = nix-update-script { };

  meta = {
    description = "xdg-desktop-portal backend for wlroots based compositors, providing screenshot and screencast";
    homepage = "https://github.com/waycrate/xdg-desktop-portal-luminous";
    license = lib.licenses.gpl3Only;
    platforms = lib.platforms.linux;
    maintainers = with lib.maintainers; [ Rishik-Y ];
  };
})
