{ nixpkgs }:
with nixpkgs;
python3Packages.buildPythonPackage rec {
	name = "affine-2.2.1";
	version = "2.2.1";
	src = fetchurl {
    url = "mirror://pypi/a/affine/${name}.tar.gz";
    sha256 = "0j3mvcnmgjvvm0znqyf7xylq7i89zjf4dq0g8280xs6bwbl5cvih";
  };

  doCheck = false;

	meta = {};
}
