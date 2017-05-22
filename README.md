# ETL Library

ETL(Extract, Transform, Load) library repo includes following libraries,

## nlp-pipeline
nlp-pipeline connects various nlp modules,
* HWordNet
* HCoreNLP
* HUKB
* predicate_matrix
* intrinio (from fetchfin repo)
* ...

## HWordNet
HWordNet is a haskell app for WordNet. It is now providing query function to
find meaning of the word by using ILI or lemmatized word.

## HCoreNLP
HCoreNLP is a haskell wrapper for Stanford CoreNLP. By adapting inline-java,
it is quite easy to use java-side functions in haskell. 

## HUKB
HUKB is a haskell wrapper for UKB. From the list of words, UKB resolves
WSD(Word Sense Disambiguation). 

## predicate_matrix
Predicate matrix is a new lexical resource resulting
from the integration of multiple sources of predicate information including
FrameNet, VerbNet, PropBank, WordNet and ESO.

Parent project is SemLink. The purpose of SemLink is the same with predicate
matrix, but it provides a far more incomplete mapping between sources.
Predicate matrix is a more complete source extended from SemLink by using
automatic predicate linking algorithm.

## intrinio
Intrinio app is retrieving data from newsapi, which provides metainfo about
titles and description from newsapi service. Currently we are using copied
data. 