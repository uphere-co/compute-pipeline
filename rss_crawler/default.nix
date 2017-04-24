{ pkgs ? import <nixpkgs> {} }:

with pkgs;

stdenv.mkDerivation {
  version = "0.1";
  name = "rss_cralwer";
  src = ./.;
  buildInputs = (with python27Packages;
                 [ 
                   requests
                   beautifulsoup4
                   redis
                 ]) 
                   ++ 
                 [
                   python27
                 ];
 installPhase = ''
   mkdir -p $out/bin/
   cp -a *.py $out/bin/
 '';

}
