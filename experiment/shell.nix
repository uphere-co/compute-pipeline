{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") {
                 inherit pkgs;
               };
    
    newHaskellPackages = haskell.packages.ghcHEAD.override {
                           overrides = self: super: {
                             vector = haskell.lib.doJailbreak super.vector;
                             syb = haskell.lib.overrideCabal super.syb { version = "0.7"; sha256 = "1da2zz7gqm4xbkx5vpd74dayx1svaxyl145fl14mq15lbb77sxdq"; } ;
                             attoparsec = haskell.lib.dontCheck super.attoparsec;
                             scientific = haskell.lib.doJailbreak (haskell.lib.dontCheck super.scientific);
                             integer-logarithms = haskell.lib.doJailbreak (haskell.lib.dontCheck super.integer-logarithms);
                             uuid-types = haskell.lib.dontCheck super.uuid-types;
                             hashable = haskell.lib.dontCheck super.hashable;
                             aeson = haskell.lib.doJailbreak (haskell.lib.dontCheck super.aeson);
                             unordered-containers = haskell.lib.dontCheck super.unordered-containers;
                             ChasingBottoms = haskell.lib.dontCheck (haskell.lib.doJailbreak super.ChasingBottoms);
                             hspec = haskell.lib.dontCheck super.hspec;
                             hspec-core = haskell.lib.dontCheck super.hspec-core;
                             hspec-discover = haskell.lib.dontCheck super.hspec-discover;
                             async = haskell.lib.doJailbreak super.async;
                             HTTP = haskell.lib.doJailbreak super.HTTP;
                             cryptohash-sha256 = haskell.lib.doJailbreak super.cryptohash-sha256;
                             hackage-security = haskell.lib.dontCheck (haskell.lib.doJailbreak super.hackage-security);
                             #cabal-install = haskell.lib.doJailbreak super.cabal-install;
                             
                           };
                         };

    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              #Cabal text aeson
              text
              scientific
              attoparsec
              aeson
            ]);
            
in stdenv.mkDerivation {
  name = "ghcHEAD-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
