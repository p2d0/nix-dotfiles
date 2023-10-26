{ pkgs, lib, makeWrapper, wrapGAppsHook, stdenv, fetchgit, ... }:

# with import <nixos-unstable> { };
stdenv.mkDerivation rec {
  pname = "gpu-screen-recorder";
  version = "master";
  src = fetchgit {
    url = "https://repo.dec05eba.com/gpu-screen-recorder";
    name = "gpu-screen-recorder";
    rev = "e0b2f0c";
    sha256 = "sha256-7kfG/M3TsVQYc+WEMuqWsZo4yQo3aLtXxfXSHMbMNp4=";
  };

  # patches = [
  #   /etc/nixos/pkgs/gpu-screen-recorder.patch
  # ];

  nativeBuildInputs = [makeWrapper wrapGAppsHook ];
  buildInputs = [
    pkgs.pkg-config
    pkgs.libdrm
    pkgs.libcap
    pkgs.libresample
    # pkgs.libav
    pkgs.libpulseaudio
    pkgs.libva
    pkgs.ffmpeg
    pkgs.xorg.xrandr
  ];

  runtimeDependencies = [
    pkgs.xorg.xrandr
    pkgs.libdrm
    pkgs.libGL
  ];

  installPhase = ''
./build.sh
mkdir -p $out/bin;
cp gsr-kms-server $out/bin
cp gpu-screen-recorder $out/bin
wrapProgram $out/bin/gsr-kms-server \
--prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [pkgs.libGL]}"
wrapProgram $out/bin/gpu-screen-recorder \
--prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [pkgs.libGL]}" \
--set KMS_SERVER "$out/bin/gsr-kms-server"
  '';

}
