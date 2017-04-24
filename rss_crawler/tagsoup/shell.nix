{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
    hsenv = haskellPackages.ghcWithPackages (p: with p; [
              aeson
	      aeson-pretty
	      base16-bytestring
	      conduit
	      conduit-extra
	      cryptohash
	      hedis
	      lens-aeson
              tagsoup
	      cabal-install 
            ]);
in stdenv.mkDerivation {
  name = "news-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
