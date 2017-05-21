{ mkDerivation, base, containers, stdenv, text }:
mkDerivation {
  pname = "predicate-matrix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers text ];
  executableHaskellDepends = [ base containers text ];
  license = stdenv.lib.licenses.unfree;
}
