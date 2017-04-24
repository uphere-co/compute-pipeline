{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let hsenv = haskellPackages.ghcWithPackages (p: with p; [
          aeson
          cabal-install
	      cassava
          Chart
          Chart-cairo
          Chart-diagrams
          conduit
          conduit-extra
	      lens
          monad-loops
	      pcre-heavy
          split
	      data-default
	    ]);
in
stdenv.mkDerivation {
  name = "ygp-ner-env";
  buildInputs =  [
		         hsenv
                 ];
  shellHook = ''
  '';
}
