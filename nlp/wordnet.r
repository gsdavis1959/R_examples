library(wordnet)
Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1")
setDict("C:/Program Files (x86)/WordNet/2.1/dict")
getDict()

if(initDict()) {
  filter <- getTermFilter("ExactMatchFilter", "company", TRUE)
  terms <- getIndexTerms("NOUN", 5, filter)
  getSynonyms(terms[[1]])
}

filter <- getTermFilter("ExactMatchFilter", "hot", TRUE)
terms <- getIndexTerms("ADJECTIVE", 1, filter)
synsets <- getSynsets(terms[[1]])
related <- getRelatedSynsets(synsets[[1]], "!")
sapply(related, getWord)

synonyms("obstruction", "NOUN")


filter <- getTermFilter("StartsWithFilter", "car", TRUE)
terms <- getIndexTerms("NOUN", 5, filter)
sapply(terms, getLemma)
