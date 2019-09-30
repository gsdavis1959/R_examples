library(NLP)
library(openNLP)
library(stringr)

tagPOS <-  function(x, ...) {
  s <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- Annotation(1L, "sentence", 1L, nchar(s))
  a2 <- annotate(s, word_token_annotator, a2)
  a3 <- annotate(s, Maxent_POS_Tag_Annotator(), a2)
  a3w <- a3[a3$type == "word"]
  POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
  POStagged <- paste(sprintf("%s/%s", s[a3w], POStags), collapse = " ")
  list(POStagged = POStagged, POStags = POStags)
}

acq <- "Gulf Applied Technologies Inc said it sold its subsidiaries engaged in pipeline and terminal operations for 12.2 mln dlrs. The company said the sale is subject to certain post closing adjustments, which it did not explain. Reuter."

acqTag <- tagPOS(acq)
acqTag <- as.character(acqTag)

verbs <- sapply(strsplit(acqTag,"[[:punct:]]*/VB.?"),function(x) {res = sub("(^.*\\s)(\\w+$)", "\\2", x); res[!grepl("\\s",res)]} )
nouns <- sapply(strsplit(acqTag,"[[:punct:]]*/NN.?"),function(x) {res = sub("(^.*\\s)(\\w+$)", "\\2", x); res[!grepl("\\s",res)]} )

verbs
nouns
