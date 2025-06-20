{ appimageTools, lib, fetchurl }:

# with import <nixos-unstable> {};
appimageTools.wrapType2 rec {
  pname = "chatgpt-pake";
  version = "1.0.0";

  src = fetchurl {
    url = "https://github.com/tw93/Pake/releases/download/V3.1.1/ChatGPT_x86_64.AppImage";
    sha256 = "sha256-Kqrl9JEk4XsfqzWrmShmE5wM5w3BC8+HODzX/Dn+92s=";
  };
}
