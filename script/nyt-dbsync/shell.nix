{ pkgs ? import <nixpkgs> {}
, ygp-webapp }:

with pkgs;

let
    hsconfig = self: super:
    {
      "product-profunctors" = self.callPackage
         ({ mkDerivation, base, contravariant, profunctors, stdenv, tagged
          , template-haskell
          }:
          mkDerivation {
            pname = "product-profunctors";
            version = "0.7.1.0";
            src = fetchgit {
              url = "git://github.com/tomjaguarpaw/product-profunctors.git";
              rev = "4029eafc74f494d5749b1bbd30bf6e87e9bb44ba";
              sha256 = "15vvm2105jwlg2kfjszl35jfavgndhv7a1cxmlpadd14mga2jzfi";
            };
            libraryHaskellDepends = [
              base contravariant profunctors tagged template-haskell
            ];
            testHaskellDepends = [ base profunctors ];
            homepage = "https://github.com/tomjaguarpaw/product-profunctors";
            description = "product-profunctors";
            license = stdenv.lib.licenses.bsd3;
          }) {};
          
      "opaleye" = self.callPackage
         ({ mkDerivation, aeson, attoparsec, base, base16-bytestring
          , bytestring, case-insensitive, containers, contravariant, multiset
          , postgresql-simple, pretty, product-profunctors, profunctors
          , QuickCheck, semigroups, stdenv, text, time, time-locale-compat
          , transformers, uuid, void
          }:
          mkDerivation {
            pname = "opaleye";
            version = "0.5.0.0";
            src = fetchgit {
              url = "git://github.com/tomjaguarpaw/haskell-opaleye.git";
              rev = "738ed9523884cb97af883222cbbe80184f8d5569";
              sha256 = "1g9y0nm2qsq5rdkifhxr89fjagnzxilf0x9lzmqpz79lcww91k0h";
            };
            libraryHaskellDepends = [
              aeson attoparsec base base16-bytestring bytestring case-insensitive
              contravariant postgresql-simple pretty product-profunctors
              profunctors semigroups text time time-locale-compat transformers
              uuid void
            ];
            testHaskellDepends = [
              base containers contravariant multiset postgresql-simple
              product-profunctors profunctors QuickCheck semigroups time
            ];
            homepage = "https://github.com/tomjaguarpaw/haskell-opaleye";
            description = "An SQL-generating DSL targeting PostgreSQL";
            license = stdenv.lib.licenses.bsd3;
            doCheck = false;
          }) {};

      "network-transport-uphere" = self.callPackage
        ({ mkDerivation, base, binary, bytestring, containers, data-accessor, distributed-process
         , network, network-simple, network-transport, network-transport-tests, stm, either
         , stdenv
         }:
         mkDerivation {
           pname = "network-transport-uphere";
           version = "0.0";
           src = fetchgit {
             url = "git://github.com/uphere-co/network-transport-uphere.git";
             rev = "5f53fb37799ca00de2e575a6ba68781759c58eb4";
             sha256 = "1vgrp6x5480p9vgk1ci7qydv76r76742inm5mzvqgxsvnyv5hj77";
           };
           libraryHaskellDepends = [
             base bytestring containers data-accessor network network-transport
           ];
           executableHaskellDepends = [
             base binary distributed-process network-simple network-transport either stm
           ];
           homepage = "http://haskell-distributed.github.com";
           description = "UpHere specific network transport";
           license = stdenv.lib.licenses.bsd3;
           doCheck = false;
         }) {};
         ygpdb = self.callPackage (ygp-webapp + "/ygpdb") {};
    };
    
    newhaskellPackages = haskellPackages.override { overrides = hsconfig; };

    hsenv = newhaskellPackages.ghcWithPackages
              (p: with p;
                [ cabal-install
                  base64-bytestring
		  either
                  lens
                  lens-aeson
                  opaleye
                  persistent-postgresql
                  product-profunctors
		  tar
                  ygpdb
		  zlib
                ]);

in stdenv.mkDerivation {
  name = "script-shell";
  buildInputs = [ hsenv ];

}
  
