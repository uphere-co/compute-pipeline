{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, bytestring, containers, directory, directory-tree, discrimination
, either, filepath, haskeline, HCoreNLP, HCoreNLP-Proto, hedis, HUKB, HUKB-driver
, HWordNet, newsapi, nyt-db, nyt-scrapper, jvm, lens, mtl, nlp-types, nlp-shared-types
, optparse-applicative, OntoNotes, postgresql-simple, predicate-matrix
, PropBank, protocol-buffers, semantic-role-labeler, stdenv, text, textview, time, time-tagger
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
    aeson attoparsec base base16-bytestring bytestring containers
    directory directory-tree discrimination either filepath haskeline
    HCoreNLP HCoreNLP-Proto hedis HUKB HUKB-driver HWordNet newsapi nyt-db nyt-scrapper jvm lens mtl nlp-types nlp-shared-types
    optparse-applicative OntoNotes postgresql-simple predicate-matrix PropBank semantic-role-labeler
    protocol-buffers text textview time time-tagger transformers yaml yayaml
    wiki-ner
  ];
  executableHaskellDepends = [ base filepath HUKB HUKB-driver text ];
  license = stdenv.lib.licenses.unfree;
}
