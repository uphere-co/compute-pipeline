# YGP-NER

YGP-NER consists of three parts.
- Numeric Entity (for e.g., Article 3 or Regulation 4-3) (call this `num`)
- Itemized Sentence (for e.g., - ITEM#N) (call this `item`)
- Utilities (call this `util`)

Logic of num and item is divided into three categories.
- Recognizer
- Annotator
- Data Sender

## Current Stage Of The App (2017-3-20)
Currently, only `is-data-sender` is in production-level.


## Nix Environment
To build all apps in YGP-NER, you should enter nix-environment.
Inside nlp-prototype/ygp_ner, 
> nix-shell shell.nix
> cabal build

## `num`
### Run `ne-annotator`

> ./dist/build/ne-annotator/ne-annotator {0 or 1}
0 : Print patterns of numeric entities on stdout
1 : Replace period inside numeric entity to space and store the replaced text with name "replaced.txt"

## `item`
### Run `is-recognizer`
> ./dist/build/is-recognizer/is-recognizer

This produces "Indicators.txt", which is input of `is-annotator` and `is-data-sender`.
"Indicators.txt" is pre-produced from "replaced.txt".

### Run `is-annotator`
> ./dist/build/is-annotator/is-annotator

Currently, this produces data for sentence distribution. The output file name is "AnnotatedSentences.txt".
"AnnotatedSentences.txt". is pre-produced from "replaced.txt" and "Indicators.txt".
Originally, this app produces the text with annotated itemized sentences.

### Run `is-data-sender`

> ./dist/build/is-data-sender/is-data-sender <input-filename>

Then JSON for metadata will be created with name <input-filename>.json .
Currently this is produced by individual input file.

In data sub-directory, <input-filename>.N files are created. Each of them are annotated
sentences by YGP-NER.


## `util`
### Run `util-analyzer`

> ./dist/build/util-analyzer/util-analyzer

By using "AnnotatedSentences.txt", this app generates Test.png, which is sentence distribution of YGP regulation text.