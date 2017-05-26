{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
, textview ? <textview>
, HCoreNLP ? <HCoreNLP>
, nlp-types ? <nlp-types>
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
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
       
  };

  myhaskellpkgs = haskell.packages.ghc802.override {
    overrides = self: super: config1 self super // config2 self super;
  }; 

  hsenv = myhaskellpkgs.ghcWithPackages (p: with p; [
            inline-java
            aeson
            attoparsec
            base16-bytestring
            data-default
            discrimination
            either
            haskeline
            lens
            monad-loops
            optparse-applicative
            postgresql-simple
            transformers
            p.textview
            p.HCoreNLP
            p.nlp-types
          ]);

in

stdenv.mkDerivation {
  name = "eventextractor-dev";
  buildInputs = [ hsenv jdk ];
  shellHook = ''
    CLASSPATH="${corenlp_models}:${corenlp}/stanford-corenlp-3.7.0.jar:${corenlp}/protobuf.jar:${corenlp}/joda-time.jar:${corenlp}/jollyday.jar:${hsenv}/share/x86_64-linux-ghc-8.0.2/HCoreNLP-0.1.0.0/HCoreNLPProto.jar";
  '';
}

