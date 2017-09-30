{ mkDerivation, base, bytestring, opaleye, stdenv, template-haskell
, text, time, uphere-opaleye
}:
mkDerivation {
  pname = "uphere-db";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring opaleye template-haskell text time uphere-opaleye
  ];
  license = stdenv.lib.licenses.unfree;
}
