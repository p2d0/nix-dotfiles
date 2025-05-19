# {
#   lib,
#   buildPythonApplication,
#   fetchFromGitHub,
#   pythonOlder,
#   fetchpatch,
#   stdenv,
#   libnotify,
#   python-telegram,
# }:

with import <nixpkgs> {};
python310Packages.buildPythonApplication rec {
  pname = "tg";
  version = "0.22.0";
  # disabled = pythonOlder "3.8";

  src = fetchGit {
    url = "https://github.com/TruncatedDinosour/arigram.git"
  }
  src = fetchFromGitHub {
    owner = "paul-nameless";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-qzqYkksocR86QFmP75ZE93kMSVmdel+OTxPgt9uZHLI=";
  };

  # patches = [
  #   # Fix sending messages
  #   # https://github.com/paul-nameless/tg/pull/306
  #   (fetchpatch {
  #     url = "https://github.com/mindtheegab/tg/commit/13e2b266989d2d757a394b0fb8cb7fd6ccc2b70c.patch";
  #     hash = "sha256-Wja6xBOlPuACzhbT8Yl3F8qSh3Kd9G1lnr9VarbPrfM=";
  #   })
  # ];

  # Fix notifications on platforms other than darwin by providing notify-send
  postPatch = lib.optionalString (!stdenv.hostPlatform.isDarwin) ''
    sed -i 's|^NOTIFY_CMD = .*|NOTIFY_CMD = "${libnotify}/bin/notify-send {title} {message} -i {icon_path}"|' tg/config.py
  '';

  propagatedBuildInputs = [ python310Packages.python-telegram python310Packages.mailcap-fix ];

  doCheck = false; # No tests

  meta = with lib; {
    description = "Terminal client for telegram";
    mainProgram = "tg";
    homepage = "https://github.com/paul-nameless/tg";
    license = licenses.unlicense;
    maintainers = with maintainers; [ sikmir ];
  };
}
