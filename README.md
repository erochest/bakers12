
# Baker's Dozen

This is a boot-camp style series of thirteen one-day coding projects. The aim
is to experiment with NLP in Haskell.

## Notes

  1. Tokenizer and application framework;
  2. Corpus processing;
  3. Search;
  4. Persistence in Level DB or SQLite in RDF;
  5. Type/token ratios, graph of changing ratio over the course of a text or
     corpus;
  6. Morphological tagger;
  7. POS tagger;
  8. Collocates, analysis and statistics;
  9. Clustering;
 10. Binary categorization (e.g., spam detection);
 11. Multi-label categorization;
 12. Hidden Markov model for corpus;
 13. Topic models;
 14. Clustering;
 15. NER, date extraction;
 16. MM text generation.

Other topics:

  * Parallel or distributed processing.

## Commands

    bakers12 init

Initialize a directory for analyzing documents.

    bakers12 add [FILE-OR-DIRECTORY] ...

This adds a document or directory of documents to the corpus.

    bakers12 info [DOCUMENT]

This prints information about a corpus or document

    bakers12 serve

This starts a [Snap](http://snapframework.com/) server for browsing information
about the corpus.


