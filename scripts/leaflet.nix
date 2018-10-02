let
  np = import <nixpkgs> { };

  ext = py-self: py-super: {
          folium = py-super.folium.overridePythonAttrs ( oldAttrs : {
                      version = "0.6.0";
            src = np.fetchFromGitHub { owner = "mpickering"
                                     ; repo = "folium"
                                     ; rev = "18d5acd71c089863c4008d8f2396297d41d4a00a"
                                     ; sha256 = "1gd3y0s517l6j55n6rda2fqi7s2jark85y0vlkx1gi185cvx9sgf";
          };
  }); };

  new-py = np.python3.override { packageOverrides = ext; };

in
  with np;
  mkShell { buildInputs = [(new-py.withPackages (ps: with ps; [ gdal folium ]))]; }

