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

  hsconfig = lib.callPackageWith (pkgs//revision) (uphere-nix-overlay + "/nix/haskell-modules/configuration-semantic-parser-api.nix")
               { inherit corenlp corenlp_models fasttext;
               };

  newHaskellPackages = haskellPackages.override { overrides = hsconfig; };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            cabal-install
            inline-java
            aeson
            attoparsec
            base16-bytestring
            beam-core
            beam-postgres
            data-default
            directory-tree
            discrimination
            distributed-process distributed-process-lifted
            haskeline
            hedis
            lens
            monad-loops
            optparse-applicative
            postgresql-simple
            servant
            servant-client
            servant-server
            transformers
            transformers-either
            yaml
            yayaml
            p.event-analyzer
            p.nlp-types
            p.nlp-shared-types
            p.textview
            p.HCoreNLP
            p.HCoreNLP-Proto
            p.HWordNet
            p.PropBank
            p.semantic-role-labeler
            p.wiki-ner
            p.uphere-network-util
            p.time-tagger
            p.OntoNotes
            p.multi-word-tagger
            p.uphere-db
          ]);

in

stdenv.mkDerivation {
  name = "nlp-pipeline-dev";
  buildInputs = [ hsenv jdk graphviz ];
  shellHook = ''
    CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.2.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}

