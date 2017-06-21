{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, fetchfin           ? <fetchfin>
, HCoreNLP           ? <HCoreNLP>
, HUKB               ? <HUKB>
, HWordNet           ? <HWordNet>
, nlp-types          ? <nlp-types>
, predicate-matrix   ? <predicate-matrix>
, PropBank           ? <PropBank>
, wiki-ner           ? <wiki-ner>
, textview           ? <textview>
}:

with pkgs;

let
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;

  config1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
               { inherit pkgs; };
  config2 =
    self: super: {
      "nlp-types" = self.callPackage (import nlp-types) {};
      "textview" = self.callPackage (import textview) {};
      "newsapi" = self.callPackage (import (fetchfin + "/newsapi")) {};
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "HCoreNLP-Proto" = self.callPackage (import (HCoreNLP + "/HCoreNLP-Proto")) {};       
      "HWordNet" = self.callPackage (import HWordNet) {};
      "predicate-matrix" = self.callPackage (import predicate-matrix) {};
      "PropBank" = self.callPackage (import PropBank) {};
      "wiki-ner" = self.callPackage (import wiki-ner) {};
  };
  ukb = import (uphere-nix-overlay + "/nix/cpp-modules/ukb.nix") { inherit stdenv fetchgit fetchurl boost; };
  config3 = import (HUKB + "/HUKB-driver/config.nix") { inherit pkgs uphere-nix-overlay ukb; };
  config4 =
    self: super: {
      "HUKB-driver" = self.callPackage (import (HUKB + "/HUKB-driver")) {}; 
    };  
  myhaskellpkgs = haskell.packages.ghc802.override {
    overrides = self: super: config1 self super // config2 self super // config3 self super // config4 self super;
  }; 



  hsenv = myhaskellpkgs.ghcWithPackages (p: with p; [
            aeson
            attoparsec
            bifunctors
            lens
            text
            cabal-install
            p.newsapi
            p.HCoreNLP
          ]);

in

stdenv.mkDerivation {
  name = "extract-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
    CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
'';
}

