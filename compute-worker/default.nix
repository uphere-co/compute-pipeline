{ mkDerivation, aeson, base, binary, bytestring, connection
, containers, data-default, deepseq, directory, distributed-process
, distributed-process-lifted, filepath, hashable, haskeline
, HCoreNLP, HFrameNet, http-client, http-client-tls, http-types
, jni, jvm, lens, lexicon-builder, monad-loops, network-simple
, network-transport, network-transport-uphere, nlp-pipeline
, nlp-shared-types, nlp-types, optparse-applicative, process
, semantic-role-labeler, semantic-types, stdenv, stm
, syntactic-analysis, text, time, transformers
, unordered-containers, uphere-network-util, wiki-ner
}:
mkDerivation {
  pname = "semantic-parser-api-compute";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring connection containers data-default
    deepseq directory distributed-process distributed-process-lifted
    filepath hashable haskeline HCoreNLP HFrameNet http-client
    http-client-tls http-types jni jvm lens lexicon-builder monad-loops
    network-simple network-transport network-transport-uphere
    nlp-pipeline nlp-shared-types nlp-types optparse-applicative
    process semantic-role-labeler semantic-types stm syntactic-analysis
    text time transformers unordered-containers uphere-network-util
    wiki-ner
  ];
  executableHaskellDepends = [
    base bytestring distributed-process distributed-process-lifted
    network-transport network-transport-uphere optparse-applicative
    text transformers
  ];
  license = stdenv.lib.licenses.unfree;
}
