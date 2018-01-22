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

  hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-semantic-parser-api.nix")
               { inherit pkgs uphere-nix-overlay event-analyzer fetchfin graph-algorithms HCoreNLP HFrameNet;
                 inherit HUKB HWordNet lexicon lexicon-builder multi-word-tagger;
                 inherit nlp-shared-types nlp-types OntoNotes PropBank;
                 inherit semantic-parser-api-compute semantic-parser-api-ghcjs;
                 inherit semantic-role-labeler semantic-types syntactic-analysis;
                 inherit textview time-tagger uphere-db uphere-network-util uphere-opaleye VerbNet wiki-ner;
                 inherit corenlp corenlp_models fetchgit fetchurl haskellPackages;
                 inherit stdenv jdk fasttext;
                 haskellLib = haskell.lib;
               };

  newHaskellPackages = haskellPackages.override { overrides = hsconfig; };

  hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
            cabal-install
            inline-java
            aeson
            attoparsec
            base16-bytestring
            data-default
            directory-tree
            discrimination
            distributed-process distributed-process-lifted
            either
            haskeline
            hedis
            lens
            monad-loops
            opaleye
            optparse-applicative
            postgresql-simple
            servant
            servant-client
            servant-server
            transformers
            yaml
            yayaml
            p.event-analyzer
            p.newsapi
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
            #p.nyt-scrapper
            p.rss-scraper
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
    CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}

