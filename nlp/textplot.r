install.packages("textplot")
install.packages('udpipe')
install.packages('igraph')
install.packages('ggraph')
install.packages('concaveman')
install.packages('BTM')
install.packages('glasso')
install.packages('qgraph')
install.packages('graph')
library(ggraph)
library(concaveman)
library(ggplot2)
library(BTM)
library(textplot)
library(udpipe)
library(igraph)
library(graph)
library(Rgraphviz)

data(brussels_listings, package = 'udpipe')
x <- table(brussels_listings$neighbourhood)
x <- sort(x)
x

textplot_bar(x, panel = "Locations", col.panel = "darkgrey", xlab = "Listings", cextext = 0.75, addpct = TRUE, cexpct = 0.5)

data(brussels_reviews_anno, package = 'udpipe')
x <- subset(brussels_reviews_anno, xpos %in% "NN" & language %in% "nl" & !is.na(lemma))
x <- document_term_frequencies(x, document = "doc_id", term = "lemma")
dtm <- document_term_matrix(x)
dtm <- dtm_remove_lowfreq(dtm, maxterms = 60)
dtm

m <- dtm_cor(dtm)
textplot_correlation_glasso(m, exclude_zero = TRUE)

sentence <- 'UDPipe provides tokenization, tagging, lemmatization and dependency parsing of raw text'
x <- udpipe(sentence, "english")
textplot_dependencyparser(x)

data(brussels_reviews_anno, package = 'udpipe')
x <- subset(brussels_reviews_anno, xpos %in% "JJ" & language %in% "fr")
x <- cooccurrence(x, group = "doc_id", term = "lemma")
x

textplot_cooccurrence(x, top_n = 25, subtitle = "showing only top 25")

