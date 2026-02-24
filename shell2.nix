{ compiler ? "ghc810"
}:
let
  pins = {
    # merge of https://github.com/NixOS/nixpkgs/pull/444862
    nixpkgs = <nixpkgs>;
      # builtins.fetchTarball {
      #   url = "https://github.com/NixOS/nixpkgs/archive/15ed8f7638116135ac9d3bd4353c482e7c539e0f.tar.gz";
      #   sha256 = "sha256:00ypnmxqm216jw55gvrh64v7shadzr16ppp3c7qpbxlkiq0mdars";
      # };

    #obelisk = import ./.obelisk/impl/thunk.nix;
  };

  nixpkgs = import pins.nixpkgs {
    inherit config;
    #overlays = [(import (pins.obelisk + "/nixpkgs-overlays/default.nix"))];
  };

  overrides = self: super:
    let
      # staticAssetsOverride =
      #   let
      #     name = "obelisk-generated-static";
      #     obelisk = import pins.obelisk {};
      #     processed = (obelisk.processAssets { src = ./static; packageName = name; }).overrideAttrs (_: _: {
      #       # Otherwise defaults to obelisk-asset-manifest from bundled 8.10 package set and rebuilds the world
      #       nativeBuildInputs = [ nixpkgs.haskell.packages.${compiler}.obelisk-asset-manifest ];
      #     });
      #   in {
      #     "${name}" = self.callCabal2nix name processed.haskellManifest {};
      #   };

    in #staticAssetsOverride //
    {
      scrappy-core = self.callCabal2nix "scrappy-core" ./. {};
      modern-uri = self.haskell.lib.doJailbreak self.modern-uri;
    };

  config = {
    packageOverrides = nixpkgs: {
      haskell = nixpkgs.haskell // {
        packages = nixpkgs.haskell.packages // {
          "${compiler}" = nixpkgs.haskell.packages.${compiler}.override(old: {
            overrides = nixpkgs.lib.foldr nixpkgs.lib.composeExtensions  (_: _: {}) [
              overrides
              #(import (pins.obelisk + "/haskell-overlays/obelisk.nix"))
            ];
          });
        };
      };
    };
  };

in {
  inherit nixpkgs;

  shell = nixpkgs.haskell.packages.${compiler}.shellFor {
    packages = p: with p; [ scrappy-core ];
    strictDeps = true;
    withHoogle = true;
    nativeBuildInputs = with nixpkgs; [
      cabal-install
      ghcid
      haskell-language-server
      hlint
    ];
  };
}
