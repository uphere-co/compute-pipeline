{ mkDerivation, aeson, base, binary, bytestring, containers
, distributed-process, filepath, hashable, jvm, lens, network
, network-simple, network-transport, network-transport-tcp, newsapi
, nlp-pipeline, nlp-shared-types, nlp-types, optparse-applicative
, postgresql-simple, servant, servant-client, servant-server
, stdenv, stm, text, time, transformers, uphere-db
, uphere-network-util, wai, wai-cors, warp, wiki-ner
}:
mkDerivation {
  pname = "nlp-query";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring containers distributed-process
    filepath hashable jvm lens network network-simple network-transport
    network-transport-tcp newsapi nlp-pipeline nlp-shared-types
    nlp-types postgresql-simple servant servant-client servant-server
    stm text time transformers uphere-db uphere-network-util wai
    wai-cors warp wiki-ner
  ];
  executableHaskellDepends = [
    base lens nlp-pipeline nlp-shared-types optparse-applicative
  ];
  license = stdenv.lib.licenses.unfree;
}
