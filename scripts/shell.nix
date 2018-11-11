with import <nixpkgs> { };

let
  pkgs1 = import (pkgs.fetchFromGitHub {
    owner = "qknight";
    repo = "nixpkgs";
    rev = "a1dd8b2a5b035b758f23584dbf212dfbf3bff67d";
    sha256 = "1zn9znsjg6hw99mshs0yjpcnh9cf2h0y5fw37hj6pfzvvxfrfp9j";
  }) {};

in

pkgs1.python3Packages.buildPythonPackage rec {
  name = "crawler";
  version = "0.0.1";

  buildInputs = [ pkgs1.firefox xorg.xorgserver pkgs1.imagemagick pkgs1.cacert ] ;

  SSL_CERT_FILE = "${cacert}/etc/ssl/certs/ca-bundle.crt";

  propagatedBuildInputs = with pkgs1.python3Packages; [
    virtual-display selenium beautifulsoup4
  ];

}
