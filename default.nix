{ mkDerivation, aeson, async, attoparsec, base, base16-bytestring
, binary, bytestring, containers, data-default, directory
, directory-tree, discrimination, either, filepath, haskeline
, HCoreNLP, HCoreNLP-Proto, HWordNet, jni, jvm, lens
, lexicon-builder, monad-loops, mtl, multi-word-tagger, newsapi
, nlp-shared-types, nlp-types
#, nyt-scrapper
, opaleye
, optparse-applicative, postgresql-simple, process
, protocol-buffers, rss-scraper, scientific, semantic-role-labeler
, split, stdenv, stm, syntactic-analysis, text, textview, time
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
    aeson async attoparsec base base16-bytestring binary bytestring
    containers data-default directory directory-tree discrimination
    either filepath haskeline HCoreNLP HCoreNLP-Proto HWordNet jni jvm
    lens lexicon-builder monad-loops mtl multi-word-tagger newsapi
    nlp-shared-types nlp-types
    #nyt-scrapper
    opaleye
    optparse-applicative postgresql-simple process protocol-buffers
    rss-scraper scientific semantic-role-labeler split stm
    syntactic-analysis text textview time time-tagger transformers
    unordered-containers uphere-db uphere-network-util vector wiki-ner
    yayaml
  ];
  executableHaskellDepends = [
    aeson base bytestring data-default filepath HCoreNLP jvm lens
    nlp-shared-types optparse-applicative split text
  ];
  license = stdenv.lib.licenses.unfree;
}
