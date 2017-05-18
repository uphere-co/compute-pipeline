# nlp-pipeline

## Usage
You can omit argument string by setting them in NIX_PATH.
> nix-shell shell.nix --argstr fetchfin <fetchfin> --argstr nlp-types <nlp-types> --argstr symbolic <symbolic> --argstr textview <textview> --argstr uphere-nix-overlay <uphere-nix-overlay> --argstr HCoreNLP <HCoreNLP> --argstr HWordNet <HWordNet>
> cabal build
> ./dist/build/nlp-pipeline/nlp-pipeline