{ stdenv, firefox, xorg, imagemagick, runCommand, python3, geckodriver, xvfb_run }:
let
  python-with-deps = python3.withPackages (ps: with ps; [virtual-display selenium beautifulsoup4]);
in

stdenv.mkDerivation {
  name = "scraper";
  version = "0.1";
  src = ./scraper.py;
  buildInputs = [ xvfb_run firefox xorg.xorgserver imagemagick geckodriver python-with-deps ];
  unpackPhase = ":";

  buildPhase = ''
    mkdir -p profile
    mkdir -p $out
    python3 $src profile $out
    mogrify -format jpg $out/*.gif
  '';

}












