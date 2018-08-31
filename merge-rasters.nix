let
  np = import <nixpkgs> { };

  ext = py-self: py-super: {
          folium = py-super.folium.overridePythonAttrs ( oldAttrs : {
                      version = "0.6.0";
                      src = np.fetchFromGitHub { owner = "mpickering"
                                               ; repo = "folium"
                                               ; rev = "027df3d30f509bee71c8093e1168f79216369b32"
                                               ; sha256 = "0mswjkq206ri25xqn7pv6r1hv8g1rbmbygjg5jpsb4a36fy7in72";
                                             }; }); };

  new-py = np.python3.override { packageOverrides = ext; };

in
  with np;
  mkShell { buildInputs = [(new-py.withPackages (ps: with ps; [ gdal shapely ]))]; }

