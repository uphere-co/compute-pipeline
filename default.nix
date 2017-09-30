{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, bytestring, containers, distributed-process, directory, directory-tree, discrimination
, either, filepath, haskeline, HCoreNLP, HCoreNLP-Proto, hedis
, HWordNet, newsapi, nyt-scrapper, jvm, lens, mtl, multi-word-tagger, network-transport, network-transport-tcp, network-util
, nlp-types, nlp-shared-types
, optparse-applicative, OntoNotes, postgresql-simple, predicate-matrix
, PropBank, protocol-buffers, rss-scraper, semantic-role-labeler, stdenv, text, textview, time, time-tagger
, transformers, yaml, yayaml
, wiki-ner
}:
mkDerivation {
  pname = "nlp-pipeline";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base16-bytestring bytestring containers distributed-process
    directory directory-tree discrimination either filepath haskeline
    HCoreNLP HCoreNLP-Proto hedis HWordNet newsapi nyt-scrapper jvm lens mtl multi-word-tagger
    network-transport network-transport-tcp network-util nlp-types nlp-shared-types
    optparse-applicative OntoNotes postgresql-simple predicate-matrix PropBank rss-scraper semantic-role-labeler
    protocol-buffers text textview time time-tagger transformers yaml yayaml
    wiki-ner
  ];
  executableHaskellDepends = [ base filepath text ];
  license = stdenv.lib.licenses.unfree;
}
