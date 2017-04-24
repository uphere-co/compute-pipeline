# Pipeline

CoreNLP-parsing for wikidata part is currently commented out. You can revert it, or simply copy /opt/wikidata.ner on mark.

## Usage
> nix-shell shell.nix
> ./dist/build/pipeline/pipeline --working-directory <working-directory-path> --minor-YGP-version <version> --minor-RSS-version <version>
> (For example) ./dist/build/pipeline/pipeline --working-directory work/ --minor-YGP-version 0 --minor-RSS-version 0

You should check if working directory exists already.

## Caution
Currently, only a child directory in pipeline works properly. For example, work/ for working-directory works.