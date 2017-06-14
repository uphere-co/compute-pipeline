{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let

hsenv = haskellPackages.ghcWithPackages (p: with p; [
          lens
          split
          hashmap
          http-client
          http-client-tls
          http-conduit
          taggy-lens
          cryptohash
          directory-tree
          base16-bytestring
        ]);

in
stdenv.mkDerivation {
  name = "news-parser-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

