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

  fasttext = import (uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit stdenv fetchgit; };
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  hsconfig = lib.callPackageWith (pkgs//revision)
               (uphere-nix-overlay + "/nix/haskell-modules/configuration-semantic-parser-api.nix") {
                 inherit corenlp corenlp_models fasttext;
                 haskellLib = haskell.lib;
               };


  newHaskellpkgs = haskellPackages.override { overrides = hsconfig; };

  hsenv = newHaskellpkgs.ghcWithPackages (p: with p; [
            inline-java
            aeson
            attoparsec
            base16-bytestring
            cabal-install
            data-default
            distributed-process distributed-process-lifted
            directory-tree
            discrimination
            either
            haskeline
            hedis
            lens
            monad-loops
            network-simple
            network-transport-tcp
            network-transport-uphere
            opaleye
            optparse-applicative
            postgresql-simple
            servant servant-client servant-server
            stm
            transformers
            wai-cors
            yaml
            yayaml
            p.HCoreNLP
            p.HCoreNLP-Proto
            p.HWordNet
            p.HUKB-driver
            p.nlp-types
            p.OntoNotes
            p.nlp-pipeline
            p.PropBank
            p.semantic-role-labeler
            p.wiki-ner
            p.textview
            p.uphere-network-util
          ]);

in

stdenv.mkDerivation {
  name = "semantic-parser-api-compute-dev";
  buildInputs = [ hsenv jdk graphviz ];
  shellHook = ''
    CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}
