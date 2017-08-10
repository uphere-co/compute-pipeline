{ mkDerivation, base, binary, bytestring, distributed-process
, network-simple, stdenv, stm
}:
mkDerivation {
  pname = "network-util";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base binary bytestring distributed-process network-simple stm
  ];
  license = stdenv.lib.licenses.unfree;
}
