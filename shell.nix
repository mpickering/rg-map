let
  np = import <nixpkgs> {};
in
  np.mkShell { buildInputs = [np.haskell.packages.ghc8104.ghc]; }
