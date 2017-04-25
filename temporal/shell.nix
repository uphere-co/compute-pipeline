{ pkgs ? import <nixpkgs> {}
, autoencode
, symbolic
, textview
, uphere-nix-overlay
, HCoreNLP
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
      "autoencode" = haskell.lib.dontHaddock (self.callPackage (import autoencode) {});
      "symbolic" = self.callPackage (import symbolic) {};
      "textview" = self.callPackage (import textview) {};
      "HCoreNLP" = self.callPackage (import HCoreNLP) { inherit jdk corenlp corenlp_models; };
       
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

  myhaskellpkgs = haskell.packages.ghc802.override {
    overrides = self: super: config1 self super // config2 self super;
  }; 

  hsenv = myhaskellpkgs.ghcWithPackages (p: with p; [
            inline-java
            aeson
            attoparsec
            base16-bytestring
            data-default
            haskeline
            lens
            monad-loops
            optparse-applicative
            postgresql-simple
            transformers
            #p.autoencode
            p.textview
            p.HCoreNLP
          ]);

in

stdenv.mkDerivation {
  name = "eventextractor-dev";
  buildInputs = [ hsenv jdk ];
  shellHook = ''
    CLASSPATH=/home/wavewave/repo/workspace/corenlp/stanford-english-corenlp-2016-10-31-models.jar:/home/wavewave/repo/workspace/corenlp/stanford-corenlp-full-2016-10-31/stanford-corenlp-3.7.0.jar:/home/wavewave/repo/workspace/corenlp/stanford-corenlp-full-2016-10-31/protobuf.jar:/home/wavewave/repo/workspace/corenlp/stanford-corenlp-full-2016-10-31/joda-time.jar:/home/wavewave/repo/workspace/corenlp/stanford-corenlp-full-2016-10-31/jollyday.jar
  '';
}

