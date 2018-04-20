{ mkDerivation, base, base16-bytestring, beam-core, beam-postgres
, bytestring, lens, microlens, postgresql-simple, stdenv, text
, time
}:
mkDerivation {
  pname = "uphere-db";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base16-bytestring beam-core beam-postgres bytestring lens
    microlens postgresql-simple text time
  ];
  license = stdenv.lib.licenses.unfree;
}
