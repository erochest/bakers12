
# Baker's Dozen

This is a boot-camp style series of thirteen one-day coding projects. The aim
is to experiment with NLP in Haskell.

## Notes

  1. Tokenizer and application framework;
  1. Type/token ratios, graph of changing ratio over the course of a text or
     corpus;
  1. Persistence in Level DB or SQLite in RDF;
  1. Snap server;
  1. Corpus processing;
  1. Search;
  1. Morphological tagger;
  1. POS tagger;
  1. Collocates, analysis and statistics;
  1. Clustering;
  1. Binary categorization (e.g., spam detection);
  1. Multi-label categorization;
  1. Hidden Markov model for corpus;
  1. Topic models;
  1. Clustering;
  1. NER, date extraction;
  1. MM text generation.

Other topics:

  * Parallel or distributed processing.

## Commands

### tokenize

    bakers12 tokenize [FILE] [...]

This tokenizes the files listed on the command line, and it prints each token
out. The output is formatted as CSV, and it includes these fields:

 * the normalized token;
 * the raw token;
 * the name of the file the token was from;
 * the offset character of the token in the file; and
 * the raw length of the token.

### Future Commands

    bakers12 init

Initialize a directory for analyzing documents.

    bakers12 add [FILE-OR-DIRECTORY] ...

This adds a document or directory of documents to the corpus.

    bakers12 info [DOCUMENT]

This prints information about a corpus or document

    bakers12 serve

This starts a [Snap](http://snapframework.com/) server for browsing information
about the corpus.


