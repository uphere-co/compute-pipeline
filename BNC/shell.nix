{ pkgs ? import <nixpkgs> {}
, uphere-nix-overlay ? <uphere-nix-overlay>
}:

with pkgs;

let
    hsconfig = import (uphere-nix-overlay + "/nix/haskell-modules/configuration-ghc-8.0.x.nix")
                 { inherit pkgs; };
   
    newHaskellPackages = haskellPackages.override {
      overrides = hsconfig;
    };
    hsenv = newHaskellPackages.ghcWithPackages (p: with p; [
              taggy-lens
            ]);
in stdenv.mkDerivation {
  name = "taggy-lens";
  buildInputs = [ hsenv ];
  shellHook = ''
  '';
}
