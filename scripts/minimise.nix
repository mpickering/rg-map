let
  np = import <nixpkgs> {};
  # The script will squash the crisper dir into this one.
  cr = import ./. {pkgs=np;};
in
  np.mkShell { buildInputs = [cr.package np.closurecompiler]; }
