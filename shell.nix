{ pkgs                  ? import <nixpkgs> {}
, event-analyzer        ? <event-analyzer>
, uphere-nix-overlay    ? <uphere-nix-overlay>
, fetchfin              ? <fetchfin>
, graph-algorithms      ? <graph-algorithms>
, HCoreNLP              ? <HCoreNLP>
, HFrameNet             ? <HFrameNet>
, HWordNet              ? <HWordNet>
, HUKB                  ? <HUKB>
, lexicon               ? <lexicon>
, lexicon-builder       ? <lexicon-builder>
, multi-word-tagger     ? <multi-word-tagger>
#, nlp-pipeline          ? <nlp-pipeline>
#, nlp-shared-types      ? <nlp-shared-types>
, nlp-types             ? <nlp-types>
, OntoNotes             ? <OntoNotes>
, predicate-matrix      ? <predicate-matrix>
, PropBank              ? <PropBank>
, semantic-role-labeler ? <semantic-role-labeler>
, semantic-types        ? <semantic-types>
, syntactic-analysis    ? <syntactic-analysis>
, textview              ? <textview>
, time-tagger           ? <time-tagger>
#, uphere-db             ? <uphere-db>
, uphere-network-util   ? <uphere-network-util>
#, uphere-opaleye        ? <uphere-opaleye>
, VerbNet               ? <VerbNet>
, wiki-ner              ? <wiki-ner>
}:

let newpkgs = import pkgs.path {
                overlays = [ (self: super: {
                               libsvm = import (uphere-nix-overlay + "/nix/cpp-modules/libsvm/default.nix") { inherit (self) stdenv fetchurl; };
                             })
                           ];
              };
in

with newpkgs;

let
  fasttext = import (uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit stdenv fetchgit; };
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;
  config1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { pkgs = newpkgs; };
  haskellPackages1 = haskellPackages.override { overrides = config1; };
  fastTextNix = import (semantic-role-labeler + "/fasttext/default.nix") {
    inherit stdenv;
    haskellPackages = haskellPackages1;
  };
  config2 =
    self: super: {
      "event-analyzer" = self.callPackage (import event-analyzer) {};
      "fastText" = self.callPackage fastTextNix { inherit fasttext; };
      "graph-algorithms" = self.callPackage (import graph-algorithms) {};
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "HCoreNLP-Proto" = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};
      "HFrameNet" = self.callPackage (import HFrameNet) {};      
      "HWordNet" = self.callPackage (import HWordNet) {};
      "lexicon" = self.callPackage (import lexicon) {};
      "lexicon-builder" = self.callPackage (import lexicon-builder) {};      
      "multi-word-tagger" = self.callPackage (import multi-word-tagger) {};
      "nlp-types" = self.callPackage (import nlp-types) {};
      "OntoNotes" = self.callPackage (import OntoNotes) {};
      "predicate-matrix" = self.callPackage (import predicate-matrix) {};
      "PropBank" = self.callPackage (import PropBank) {};
      "rss-scraper" = self.callPackage (import (fetchfin + "/rss-scraper")) {};
      "semantic-role-labeler" = self.callPackage (import semantic-role-labeler) {};
      "semantic-types" = self.callPackage (import semantic-types) {};
      "syntactic-analysis" = self.callPackage (import syntactic-analysis) {};
      "textview" = self.callPackage (import textview) {};
      "time-tagger" = self.callPackage (import time-tagger) {};      
      "uphere-network-util" = self.callPackage (import uphere-network-util) {};      
      "uphere-opaleye" = self.callPackage (import uphere-opaleye) {};
      "VerbNet" = self.callPackage (import VerbNet) {};
      "wiki-ner" = self.callPackage (import wiki-ner) {};
      
      #"nlp-shared-types" = self.callPackage (import nlp-shared-types) {};
      #"uphere-db" = self.callPackage (import uphere-db) {};
      #"newsapi" = self.callPackage (import (fetchfin + "/newsapi")) {};      
      #"nlp-pipeline" = self.callPackage (import nlp-pipeline) {};


      };

  ukb = import (uphere-nix-overlay + "/nix/cpp-modules/ukb.nix") { inherit stdenv fetchgit fetchurl boost; };

  config3 = import (HUKB + "/HUKB-driver/config.nix") { pkgs = newpkgs; inherit uphere-nix-overlay ukb; };
  config4 =
    self: super: {
      "HUKB-driver" = self.callPackage (import (HUKB + "/HUKB-driver")) {};
    };

  newHaskellpkgs = haskellPackages.override {
    overrides = self: super: config1 self super // config2 self super // config3 self super // config4 self super;
  }; 

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
            p.predicate-matrix
            p.PropBank
            p.semantic-role-labeler
            p.wiki-ner
            p.textview
            p.time-tagger
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

