let

myHaskellPackageOverlay = self: super: {

  myHaskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: rec {

      cas-store = self.haskell.lib.overrideCabal self.haskellPackages.cas-store (old: { libraryHaskellDepends = old.libraryHaskellDepends ++ [ self.haskellPackages.kqueue ]; });

      funflow = super.haskell.lib.dontCheck (hself.callHackage "funflow" "1.5.0" {});

      funflow-nix = super.haskell.lib.dontCheck hsuper.funflow-nix;


    };
  };
};

in

{ nixpkgs ? import <nixpkgs> { config.allowBroken = true;
                               overlays = [ myHaskellPackageOverlay ];
                             },
  compiler ? "default",
  doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, data-default, funflow
      , funflow-nix, Glob, lib, path, path-io, time, unix
      }:
      mkDerivation {
        pname = "map-scraper";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers data-default funflow funflow-nix Glob path path-io
          time unix
        ];
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (pkgs.myHaskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
