{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;
  wikiScraper = import ./default.nix;
  drv = variant (haskellPackages.callPackage wikiScraper {}); 
in
pkgs.haskell.packages.ghc910.shellFor {
  packages = p: with p; [ drv ];
  strictDeps = true;
  withHoogle = true;
  nativeBuildInputs = with nixpkgs; [
    cabal-install
    ghcid
    haskell-language-server
    hlint
  ];
}   


# pkgs.mkShell {
#   buildInputs = [ 
#     pkgs.cabal-install 
#     pkgs.haskell-language-server
#     pkgs.ghcid
#     pkgs.hlint
#   ];
#   inputsFrom = [ (if pkgs.lib.inNixShell then drv.env else drv) ];
# } 



