install.packages("ruimtehol")
vignette("ground-control-to-ruimtehol", package = "ruimtehol")
library(ruimtehol) 
data("dekamer", package = "ruimtehol") 
str(dekamer)
dekamer$x <- strsplit(dekamer$question, "\\W") 
dekamer$x <- lapply(dekamer$x, FUN = function(x) setdiff(x, "")) 
dekamer$x <- sapply(dekamer$x, FUN = function(x) paste(x, collapse = " ")) 
dekamer$x <- tolower(dekamer$x) 
dekamer$y <- strsplit(dekamer$question_theme, split = ",") 
dekamer$y <- lapply(dekamer$y, FUN=function(x) gsub(" ", "-", x)) 
dekamer$x[1:2]
dekamer$y[1:2]

# model
set.seed(321) 
model <- embed_tagspace(x = dekamer$x, y = dekamer$y, 
                              early_stopping = 0.8, validationPatience = 10, 
                              dim = 50, 
                              lr = 0.01, epoch = 40, loss = "softmax", adagrad = TRUE, 
                              similarity = "cosine", negSearchLimit = 50, 
                              ngrams = 2, minCount = 2)
model

# plot
plot(model)

# dictionary of similarity
dict <- starspace_dictionary(model) 
str(dict)
head(dict)

# embeddings
emb <- as.matrix(model) 
dim(emb)
emb_words <- as.matrix(model, type = "words") 
emb_labels <- as.matrix(model, type = "labels", prefix = FALSE) 
e <- starspace_embedding(model, x = c("__label__VERVOERBELEID", "geld"), type = "ngram")
e
# embeddings of full tex
text <- c("de nmbs heeft het treinaanbod uitgebreid via onteigening ...", "de migranten komen naar europa de asielcentra ...") 
emb_text <- starspace_embedding(model, text) 
dim(emb_text)

# predictions
predict(model, "de migranten komen naar europa de asielcentra ...")
embedding_similarity(emb_text, emb_labels, type = "cosine", top_n = 5)
starspace_knn(model, "de migranten komen naar europa de asielcentra ...", k = 5)

targetdocs <- c("__label__FISCALITEIT", 
                "__label__OVERHEIDSADMINISTRATIE", 
                "__label__MIGRATIEBELEID", "__label__POLITIE", 
                "__label__BUITENLANDS-BELEID", 
                "__label__ECONOMISCH-BELEID", 
                "de migranten komen naar europa ZZZ", 
                "__label__PERSONEEL") 
predict(model, "de migranten komen naar europa de asielcentra ...", basedoc = targetdocs)
embedding_similarity( starspace_embedding(model, "de migranten komen naar europa de asielcentra ..."), starspace_embedding(model, targetdocs), top_n = 3)

setwd('~/Data/RStatistics/WindowsRSource/Text')
starspace_save_model(model, file = "textspace.ruimtehol") 
model <- starspace_load_model("textspace.ruimtehol")
