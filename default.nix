{ mkDerivation, base, bytestring, lens, opaleye, postgresql-simple
, stdenv, template-haskell, text, time, uphere-opaleye
}:
mkDerivation {
  pname = "uphere-db";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring lens opaleye postgresql-simple template-haskell
    text time uphere-opaleye
  ];
  license = stdenv.lib.licenses.unfree;
}
