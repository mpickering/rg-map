let
  np = import <nixpkgs> { };
  affine = import ./affine.nix { nixpkgs = np; };

in
  with np;
  mkShell { buildInputs = [(python3.withPackages (ps: with ps; [ affine pillow folium pyproj ]))]; }

