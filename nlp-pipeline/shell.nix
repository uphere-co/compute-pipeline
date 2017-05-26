{ pkgs ? import <nixpkgs> {}
, fetchfin ? <fetchfin>
, nlp-types ? <nlp-types>
, textview ? <textview>
, uphere-nix-overlay ? <uphere-nix-overlay>
, HCoreNLP ? <HCoreNLP>
, HWordNet ? <HWordNet>
, HUKB ? <HUKB>
}:

with pkgs;

let
  res_corenlp = import (uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
    inherit fetchurl fetchzip srcOnly;
  };
  corenlp = res_corenlp.corenlp;
  corenlp_models = res_corenlp.corenlp_models;
  config1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { inherit pkgs; };
  config2 =
    self: super: {
      "nlp-types" = self.callPackage (import nlp-types) {};
      "textview" = self.callPackage (import textview) {};
      "intrinio" = self.callPackage (import (fetchfin + "/intrinio")) {};
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "HWordNet" = self.callPackage (import HWordNet) {};
      "predicate-matrix" = self.callPackage ../predicate_matrix {};
       
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
            inline-java
            aeson
            attoparsec
            base16-bytestring
            cabal-install
            data-default
            directory-tree
            discrimination
            either
            haskeline
            lens
            monad-loops
            optparse-applicative
            postgresql-simple
            transformers
            yaml
            yayaml
            p.intrinio
            p.nlp-types
            p.textview
            p.HCoreNLP
            p.HWordNet
            p.predicate-matrix
            HUKB-driver
          ]);

in

stdenv.mkDerivation {
  name = "eventextractor-dev";
  buildInputs = [ hsenv jdk ukb ];
  shellHook = ''
    CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}

