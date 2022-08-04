{ picom, lib, fetchFromGitHub, ... }:

picom.overrideAttrs (oldAttrs: rec {
  pname = "picom-animations";
  version = "unstable-2022-02-05";
  src = fetchFromGitHub {
    owner = "dccsillag";
    repo = "picom";
    rev = "implement-window-animations";
    sha256 = "sha256-crCwRJd859DCIC0pEerpDqdX2j8ZrNAzVaSSB3mTPN8=";
  };
})
