let
  np = import <nixpkgs> { };

  ext = py-self: py-super: {
          folium = py-super.folium.overridePythonAttrs ( oldAttrs : {
                      version = "0.6.0";
                      src = np.fetchFromGitHub { owner = "mpickering"
                                               ; repo = "folium"
                                               ; rev = "d7bfd3e586b2e22e9bbd78feafe6e5005c98cd4f"
                                               ; sha256 = "1176f42pag55i9d27z6pv755xs1z9b7hb1z5m880qva44sf0ybh2";
                                             }; }); };

  new-py = np.python3.override { packageOverrides = ext; };

in
  with np;
  mkShell { buildInputs = [(new-py.withPackages (ps: with ps; [ gdal folium ]))]; }

