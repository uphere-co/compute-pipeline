{ pkgs ? import <nixpkgs> {}
}:

with pkgs;

let

  hsenv = haskellPackages.ghcWithPackages (p: with p; [
            lens
            split
            http-client
              http-client-tls
                            http-conduit
                        taggy-lens
          ]);

in

stdenv.mkDerivation {
  name = "investopedia-dev";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}

