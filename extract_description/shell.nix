{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, fetchfin           ? <fetchfin>
}:

with pkgs;

let
  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };

  config2 =
    self: super: {
      "newsapi" = self.callPackage (import (fetchfin + "/newsapi")) {};
    };

  newHaskellPackages = haskellPackages.override {
    overrides = self: super: hsconfig self super // config2 self super;
  };


  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            aeson
            attoparsec
            bifunctors
            lens
            text
            cabal-install
            p.newsapi
          ]);

in

stdenv.mkDerivation {
  name = "extract-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

