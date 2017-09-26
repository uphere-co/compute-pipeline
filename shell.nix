{ pkgs               ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, uphere-opaleye     ? <uphere-opaleye>
}:

with pkgs;

let hsconfig = self: super:
      let
        config1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { inherit pkgs; };
      in config1 self super //
      {
        "uphere-opaleye" = self.callPackage (import uphere-opaleye) {};
      };
    newHaskellPackages = haskellPackages.override {
                           overrides = hsconfig;
                         };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              aeson
              aeson-pretty
              base16-bytestring
              base64-bytestring
              cabal-install
              cryptohash
              data-default
              directory-tree
              haskeline
              http-client
              http-client-tls
              http-conduit
              monad-loops
              opaleye
              optparse-applicative
              split
              string-conversions
              text
              text-format
              uniplate
              unix
              p.uphere-opaleye
            ]);
in stdenv.mkDerivation {
  name = "uphere-db-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
