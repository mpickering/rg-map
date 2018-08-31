let
  np = import <nixpkgs> { };

  ext = py-self: py-super: {
          folium = py-super.folium.overridePythonAttrs ( oldAttrs : {
                      version = "0.6.0";
                      src = np.fetchFromGitHub { owner = "mpickering"
                                               ; repo = "folium"
                                               ; rev = "d7bfd3e586b2e22e9bbd78feafe6e5005c98cd4f"
                                               ; sha256 = "1qdhvqi7vagqbkh5v6lxmcv1ms5ixyll72v7qh05c1ans1prx4xa";
                                             }; }); };

  new-py = np.python3.override { packageOverrides = ext; };

in
  with np;
  mkShell { buildInputs = [(new-py.withPackages (ps: with ps; [ gdal folium ]))]; }

