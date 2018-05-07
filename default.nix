{ mkDerivation, aeson, aeson-pretty, async, attoparsec, base
, base16-bytestring, beam-core, beam-postgres, binary, bytestring
, containers, data-default, directory, directory-tree
, discrimination, errors, event-analyzer, filepath
, graph-algorithms, haskeline, HCoreNLP, HCoreNLP-Proto, HWordNet
, inline-java, jni, jvm, lens, lexicon-builder, monad-loops
, multi-word-tagger, nlp-shared-types, nlp-types
, optparse-applicative, postgresql-simple, process
, protocol-buffers, scientific, semantic-role-labeler, split
, stdenv, stm, syntactic-analysis, text, textview, time
, time-tagger, transformers, transformers-either
, unordered-containers, uphere-db, uphere-network-util, vector
, wiki-ner, yayaml
}:
mkDerivation {
  pname = "nlp-pipeline";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty async attoparsec base base16-bytestring
    beam-core beam-postgres binary bytestring containers data-default
    directory directory-tree discrimination errors event-analyzer
    filepath graph-algorithms haskeline HCoreNLP HCoreNLP-Proto
    HWordNet inline-java jni jvm lens lexicon-builder monad-loops
    multi-word-tagger nlp-shared-types nlp-types optparse-applicative
    postgresql-simple process protocol-buffers scientific
    semantic-role-labeler split stm syntactic-analysis text textview
    time time-tagger transformers transformers-either
    unordered-containers uphere-db uphere-network-util vector wiki-ner
    yayaml
  ];
  executableHaskellDepends = [ base lens optparse-applicative ];
  license = stdenv.lib.licenses.unfree;
}
