{ pkgs, ... }:

# with import <nixos-unstable> {};
with pkgs;
let pygments = (pkgs.python310Packages.pygments.overrideAttrs(oldAttrs: rec {
      pname = "pygments";
      version = "2.14.0";
      src = python.pkgs.fetchPypi {
        pname = "Pygments";
        inherit version;
        sha256 = "sha256-s+0GqeismpquWm9dvniopYZV0XtDuTwHjwlN3Edq4pc=";
      };
    }));
in
python310.pkgs.buildPythonPackage rec {
  pname = "shell_gpt";
  version = "0.7.0";
  src = python.pkgs.fetchPypi {
    inherit pname version;
    sha256 = "sha256-1PCwMMhNkZ+eBn7yje/r9XoXQ/IUnEfMWCzynM/b1cc=";
  };
  doCheck = false;
  propagatedBuildInputs = [
    # Specify dependencies
    pkgs.python310Packages.requests
    (pkgs.python310Packages.rich.overrideAttrs(oldAttrs: rec {
      pname = "rich";
      version = "13.3.1";
      propagatedBuildInputs = [
        pkgs.python310Packages.markdown-it-py
        pygments
      ];

      src = fetchFromGitHub {
        owner = "Textualize";
        repo = pname;
        rev = "v${version}";
        sha256 = "sha256-1soeb3aD4wB4stILvfOga/YZtyH6jd0XvnxkLmbW4G0=";
      };
    }))
    pkgs.python310Packages.typer
  ];
}
