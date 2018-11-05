{ revision }:

let
  reflex-platform-src =
     (import (revision.uphere-nix-overlay + "/nix/web-modules/reflex.nix") {}).reflex-platform-src;
  reflex-platform = import reflex-platform-src {};

in

reflex-platform.project (

{ pkgs, ... }:

let

  fasttext = import (revision.uphere-nix-overlay + "/nix/cpp-modules/fasttext.nix") { inherit (pkgs) stdenv fetchgit; };

  corenlp_pkgs =
    import (revision.uphere-nix-overlay + "/nix/linguistic-resources/corenlp.nix") {
      inherit (pkgs) fetchurl fetchzip srcOnly;
    };

  env-hook-gen =
    haskellPackages:
      import (revision.uphere-nix-overlay + "/nix/env/corenlp.nix") {
        inherit (pkgs) makeSetupHook writeText;
        inherit (haskellPackages) HCoreNLP;
        inherit (corenlp_pkgs) corenlp corenlp_models;
      };

  hsconfig = pkgs.lib.callPackageWith
               (pkgs//revision)
               (revision.uphere-nix-overlay + "/nix/haskell-modules/configuration-compute-pipeline.nix")
               {
                 inherit fasttext;
                 inherit (corenlp_pkgs) corenlp corenlp_models;
               };

in


{
  packages = {
    cloud-haskell-util   = ./cloud-haskell-util;
    compute-so           = ./compute-so;
    compute-so-types     = ./compute-so-types;
    compute-worker       = ./compute-worker;
    jobqueue-client      = ./jobqueue-client;
    jobqueue-server      = ./jobqueue-server;
    jobqueue-types       = ./jobqueue-types;
    storage-manager      = ./storage-manager;
    task-corenlp         = ./task-corenlp;
    task-reuters         = ./task-reuters;
    task-semantic-parser = ./task-semantic-parser;
    uphere-db            = ./uphere-db;
  };


  overrides =
    self: super:
    hsconfig self super
    //
    {

      reflex-dom-contrib = self.callCabal2nix
        "reflex-dom-contrib"
        (pkgs.fetchFromGitHub {
          owner = "reflex-frp";
          repo = "reflex-dom-contrib";
          rev = "b47f90c810c838009bf69e1f8dacdcd10fe8ffe3";
          sha256 = "0yvjnr9xfm0bg7b6q7ssdci43ca2ap3wvjhshv61dnpvh60ldsk9";
        }) {};

      reflex-dom-nested-routing = self.callCabal2nix
        "reflex-dom-nested-routing"
        (pkgs.fetchFromGitHub {
          owner = "3noch";
          repo = "reflex-dom-nested-routing";
          rev = "c49c75c693de8516d1b19314be500482bea9426c";
          sha256 = "00bmakqm9893h8l3w7l1r1fjkpyffifcaicqmj2q5wwlfvm96hbf";
        }) {};

      wai-middleware-etag = pkgs.haskell.lib.doJailbreak super.wai-middleware-etag;



    };

  tools = ghc: let env-hook = env-hook-gen ghc;
               in [ env-hook ];  # NOTE: you cannot have non-variable in this list.

  shells = {
    ghc = [
      "cloud-haskell-util"
      "compute-so"
      "compute-so-types"
      "compute-worker"
      "jobqueue-client"
      "jobqueue-server"
      "jobqueue-types"
      "storage-manager"
      "task-corenlp"
      "task-reuters"
      "task-semantic-parser"
      "uphere-db"
    ];
  };
})
