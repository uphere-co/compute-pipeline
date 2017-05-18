# nlp-pipeline

From text corpus, we need to produce annotated text for event extraction.

By using HCoreNLP, currently possible annotations are lemmatization, temporal
expression and POS tagging. Currently, UKB input data from text is necessary for
feeding FrameNet reinforced by predicate matrix.

nlp-pipeline is to read text corpus, to produce annotated text for communication between
various language modules, and to print internal data in a pretty form for human-read(
currently YAYAML is being used for this purpose).

## Usage
You can omit argument string by setting them in NIX_PATH.
> nix-shell shell.nix --argstr fetchfin <fetchfin> --argstr nlp-types <nlp-types> --argstr symbolic <symbolic> --argstr textview <textview> --argstr uphere-nix-overlay <uphere-nix-overlay> --argstr HCoreNLP <HCoreNLP> --argstr HWordNet <HWordNet>
> cabal build
> ./dist/build/nlp-pipeline/nlp-pipeline