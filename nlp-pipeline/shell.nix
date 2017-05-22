{ pkgs ? import <nixpkgs> {}
, fetchfin ? <fetchfin>
, nlp-types ? <nlp-types>
, symbolic ? <symbolic>
, textview ? <textview>
, uphere-nix-overlay ? <uphere-nix-overlay>
, HCoreNLP ? <HCoreNLP>
, HWordNet ? <HWordNet>
, HUKB ? <HUKB>
}:

with pkgs;

let
  inline-java-src = fetchgit {
    url = "git://github.com/wavewave/inline-java.git";
    rev = "593cdf3a02a866c6822539c0e89adc8ed913a9ba";
    sha256 = "1xngx5i7gpg4h33w6iznrphd1ji0f8dmf5lb5awsnxp72kszvqi5";
  };
  corenlp = srcOnly {
    name = "corenlp-20161031";
    src = fetchzip {
      url = "http://nlp.stanford.edu/software/stanford-corenlp-full-2016-10-31.zip";
      sha256 = "0lm4rhvhfi54y01ad40g3v9qdw5qk5982fqfa465x2s9g7fxknmv";
    };
  };
  corenlp_models = fetchurl {
    url = "http://nlp.stanford.edu/software/stanford-english-corenlp-2016-10-31-models.jar";
    sha256 = "1jl86fgqcbrhmp000id705wx131j4zcmm70q7pprgj5zyjp32zxm";
    
  };
  config1 = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix") { inherit pkgs; };
  config2 =
    self: super: {
      "nlp-types" = self.callPackage (import nlp-types) {};
      "symbolic" = self.callPackage (import symbolic) {};
      "textview" = self.callPackage (import textview) {};
      "intrinio" = self.callPackage (import (fetchfin + "/intrinio")) {};
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
      "HWordNet" = self.callPackage (import HWordNet) {};
      "predicate-matrix" = self.callPackage ../predicate_matrix {};
       
      "inline-java" = self.callPackage
        ({ mkDerivation, base, binary, bytestring, Cabal, containers
         , directory, distributed-closure, filepath, ghc-heap-view, hspec
         , inline-c, jni, jvm, language-java, process, singletons, stdenv
         , syb, template-haskell, temporary, text, thread-local-storage
         , vector
         , jdk
         }:
         mkDerivation {
           pname = "inline-java";
           version = "0.6.2";
           src = "${inline-java-src}";
           libraryHaskellDepends = [
             base binary bytestring Cabal containers directory
             distributed-closure filepath ghc-heap-view inline-c jni jvm
             language-java process singletons syb template-haskell temporary
             text thread-local-storage vector
           ];
           testHaskellDepends = [
             base bytestring hspec jni jvm singletons text
           ];
           buildDepends = [ jdk ];
           homepage = "http://github.com/tweag/inline-java#readme";
           description = "Java interop via inline Java code in Haskell modules";
           license = stdenv.lib.licenses.bsd3;
         }) { jdk = pkgs.jdk; };

      "jvm" = self.callPackage
        ({ mkDerivation, base, bytestring, distributed-closure, hspec, jni
         , singletons, stdenv, text, vector
         , jdk
         }:
         mkDerivation {
           pname = "jvm";
           version = "0.2.0";
           src = "${inline-java-src}/jvm";
           libraryHaskellDepends = [
             base bytestring distributed-closure jni singletons text vector
           ];
           testHaskellDepends = [ base bytestring hspec text ];
           buildDepends = [ jdk ];           
           #doCheck = false;
           homepage = "http://github.com/tweag/inline-java/tree/master/jvm#readme";
           description = "Call JVM methods from Haskell";
           license = stdenv.lib.licenses.bsd3;
         }) { jdk = pkgs.jdk; };

      "jni" = self.callPackage
        ({ mkDerivation, base, bytestring, choice, containers, inline-c
         , singletons, thread-local-storage
         , cpphs, jdk
         }:
         mkDerivation {
           pname = "jni";
           src = "${inline-java-src}/jni";
           version = "0.3.0";
           libraryHaskellDepends = [
             base bytestring choice containers inline-c singletons thread-local-storage
           ];
           setupHaskellDepends = [ cpphs ];
           configureFlags = ["--extra-lib-dirs=${jdk.jre}/lib/openjdk/jre/lib/amd64/server"]; 
           librarySystemDepends = [ jdk ];
           homepage = "https://github.com/tweag/inline-java/tree/master/jni#readme";
           description = "Complete JNI raw bindings";
           license = stdenv.lib.licenses.bsd3;
           hydraPlatforms = stdenv.lib.platforms.none;
         }) {jdk = pkgs.jdk;};
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

