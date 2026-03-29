{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  haskellLib = pkgs.haskell.lib;
  henforcer = haskellLib.dontCheck (haskellLib.doJailbreak (haskellPackages.callPackage ({ mkDerivation, base, containers, dlist, fetchzip, filepattern, ghc
    , lib, optparse-applicative, pollock, text, tomland
    }:
    mkDerivation {
      pname = "henforcer";
      version = "1.0.0.1";
      src = fetchzip {
        url = "https://hackage.haskell.org/package/henforcer-1.0.0.1/henforcer-1.0.0.1.tar.gz";
        sha256 = "1vnwvhszvqrypcvk8zvb7px2q78mn3lhbhj521vsrz6zj6459f0r";
      };
      libraryHaskellDepends = [
        base containers dlist filepattern ghc optparse-applicative pollock
        text tomland
      ];
      doHaddock = false;
      license = lib.licenses.mit;
    }) {}));
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  pkg = import ./default.nix;
  drv = variant (haskellPackages.callPackage pkg { inherit henforcer; });
in
pkgs.haskell.packages.ghc910.shellFor {
  packages = p: with p; [ drv ];
  strictDeps = true;
  withHoogle = true;
  nativeBuildInputs = with nixpkgs; [
    cabal-install
    haskellPackages.fourmolu
    ghcid
    haskell-language-server
    hlint
  ];
}
