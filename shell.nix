{ revision }:

with revision;

let pkgs0 = import nixpkgs { config.allowUnfree = true; };

    pkgs = import pkgs0.path {
                overlays = [ (self: super: {
                               libsvm = import (uphere-nix-overlay + "/nix/cpp-modules/libsvm/default.nix") { inherit (self) stdenv fetchurl; };
                             })
                           ];
              };
in

with pkgs;

let
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = lib.callPackageWith (pkgs//revision) (uphere-nix-overlay + "/nix/haskell-modules/configuration-semantic-parser-api.nix") {
    inherit corenlp corenlp_models;
    fasttext = null;
    haskellLib = haskell.lib;
  };

  newHaskellPackages = haskellPackages.override { overrides = hsconfig; };

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
              lens
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
