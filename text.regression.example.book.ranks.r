library(tm)
setwd("~/Data/RStatistics/R_Source/machine.learning.for.hackers/06-Regularization")

ranks <- read.csv(file.path('data', 'oreilly.csv'),
                  stringsAsFactors = FALSE)



documents <- data.frame(Text = ranks$Long.Desc.)
row.names(documents) <- 1:nrow(documents)

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, content_transformer(tolower))

dtm <- DocumentTermMatrix(corpus)
dtm2 <- removeSparseTerms(dtm, sparse=0.70)

# Twenty-seventh code snippet
x <- as.matrix(dtm2)
xy <- rev(1:100)

xy <- na.omit(as.data.frame(merge(ranks$Rank, x, by="row.names")))
xy$Rank <- as.numeric(xy$x)
xy <- xy[,c(3:23)]

require(psych)
require(corrplot)
corMat <- cor(xy)
corrplot(corMat)


pc.model <- princomp(corMat)
summary(pc.model)
loadings(pc.model)
pred1 <- lm(Rank ~ ., data=xy)
summary(pred1)
