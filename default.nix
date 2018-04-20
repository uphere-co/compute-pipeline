{ mkDerivation, aeson, async, attoparsec, base, base16-bytestring
, beam-core, beam-postgres, binary, bytestring, containers
, data-default, directory, directory-tree, discrimination, either
, event-analyzer, filepath, graph-algorithms, haskeline, HCoreNLP
, HCoreNLP-Proto, HWordNet, jni, jvm, lens, lexicon-builder
, monad-loops, mtl, multi-word-tagger, nlp-shared-types, nlp-types
, optparse-applicative, postgresql-simple, process
, protocol-buffers, scientific, semantic-role-labeler, split
, stdenv, stm, syntactic-analysis, text, textview, time
, time-tagger, transformers, unordered-containers, uphere-db
, uphere-network-util, vector, wiki-ner, yayaml
}:
mkDerivation {
  pname = "nlp-pipeline";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async attoparsec base base16-bytestring beam-core
    beam-postgres binary bytestring containers data-default directory
    directory-tree discrimination either event-analyzer filepath
    graph-algorithms haskeline HCoreNLP HCoreNLP-Proto HWordNet jni jvm
    lens lexicon-builder monad-loops mtl multi-word-tagger
    nlp-shared-types nlp-types optparse-applicative postgresql-simple
    process protocol-buffers scientific semantic-role-labeler split stm
    syntactic-analysis text textview time time-tagger transformers
    unordered-containers uphere-db uphere-network-util vector wiki-ner
    yayaml
  ];
  executableHaskellDepends = [ base lens optparse-applicative ];
  license = stdenv.lib.licenses.unfree;
}
