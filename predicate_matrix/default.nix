{ mkDerivation, base, containers, stdenv, lens, text }:
mkDerivation {
  pname = "predicate-matrix";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base containers lens text ];
  executableHaskellDepends = [ base containers lens text ];
  license = stdenv.lib.licenses.unfree;
}
