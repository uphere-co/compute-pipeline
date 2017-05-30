{ mkDerivation, aeson, attoparsec, base, base16-bytestring
, bytestring, containers, directory, directory-tree, discrimination
, either, filepath, haskeline, HCoreNLP, HCoreNLP-Proto, HUKB, HUKB-driver
, HWordNet, intrinio, jvm, lens, mtl, nlp-types
, optparse-applicative, postgresql-simple, predicate-matrix
, PropBank, protocol-buffers, stdenv, text, textview, time
, transformers, yaml, yayaml
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
    HCoreNLP HCoreNLP-Proto HUKB HUKB-driver HWordNet intrinio jvm lens mtl nlp-types
    optparse-applicative postgresql-simple predicate-matrix PropBank
    protocol-buffers text textview time transformers yaml yayaml
  ];
  executableHaskellDepends = [ base filepath HUKB HUKB-driver text ];
  license = stdenv.lib.licenses.unfree;
}
