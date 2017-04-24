{ pkgs ? import <nixpkgs> {}
, nlp-prototype 
}:

with pkgs;

let toolz     = callPackage (nlp-prototype + "/nix/default-python.nix") {
                  inherit pkgs;
                  buildPythonPackage = pkgs.python27Packages.buildPythonPackage;
                };
    toolz_cpp = callPackage (nlp-prototype + "/nix/default-cpp.nix") { };                   
    hsenv = haskellPackages.ghcWithPackages (p: with p; [
              aeson
              cabal-install
              cassava
              conduit
              conduit-extra
              lens
              monad-loops
              optparse-applicative
              split
              data-default
            ]);
in
stdenv.mkDerivation {
  name = "pipeline-env";
  buildInputs =
  (with python27Packages;
                 [
                   line_profiler matplotlib seaborn
                   numpy scipy pandas scikitlearn
                   pyzmq redis cython numba
                   toolz.gensim toolz.untangle
                   h5py pytest toolz.pytest-mock
                   toolz.guppy toolz.nltk toolz.bllipparser
                   psycopg2
                   toolz.cldoc toolz.feedparser beautifulsoup4
                 ])
                 ++
                 [
                   wget jdk zip unzip which stress htop
                   cmake pkgconfig clang clang-analyzer elfutils
                   gcc6
                   boost
                   hdf5 hdf5-cpp liblbfgs
                   cppzmq zeromq
                   tbb openblas
                   linuxPackages.perf
                   toolz_cpp.msgsl
                   toolz_cpp.spdlog
                   toolz_cpp.flatbuffers
                   toolz_cpp.fmt
                   toolz_cpp.json
                   toolz_cpp.csv
                   toolz_cpp.backwardcpp
                   toolz_cpp.xxhashct
                   toolz_cpp.variant
                   doxygen graphviz
                   libcgroup
                   redis
                   libpqxx
                   ucspi-tcp
                   jq pigz
                 ] ++
                 [
                         hsenv
                 ];
  shellHook = ''
    CC=clang
    CXX=clang++
    MODEL=/data/groups/uphere/parsers/corenlp/
    CORENLP=/data/groups/uphere/parsers/corenlp/
    PARSER=/data/groups/uphere/parsers/corenlp/stanford-parser-full-2015-12-09/
    CLASSPATH=$CORENLP/stanford-corenlp.Oct2016.jar:$MODEL/stanford-english-corenlp-2016-01-10-models.jar:$CORENLP/stanford-ner.jar;
  '';
}
