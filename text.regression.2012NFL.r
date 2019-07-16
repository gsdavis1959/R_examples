rm(list = ls(all = TRUE))
library(tm)
library(tidyverse)
library(data.table)
setwd("~/Data//Datasets/Sports")

file <- read.csv("2012_nfl_pbp_data_reg_season.csv",
                  stringsAsFactors = FALSE)




documents <- data.frame(Text = file$description)
setDT(documents, keep.rownames = TRUE)[]



documents <- documents %>% 
  rename(doc_id = rn) %>%
  glimpse()


head(documents)
corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

dtm <- DocumentTermMatrix(corpus)
dtm2 <- removeSparseTerms(dtm, sparse=0.95)

# Twenty-seventh code snippet
x <- as.matrix(dtm2)


xy <- na.omit(as.data.frame(merge(file, x, by="row.names")))

xy$Gross <- as.numeric(xy$x)
xy <- xy[,c(3:10)]

require(psych)
require(corrplot)

str(xy)

pred1 <- lm(teamwin ~ offscore * defscore, data=xy)
summary(pred1)
corMat <- cor(xy)
corrplot(corMat)

pc.model <- princomp(corMat)
summary(pc.model)
loadings(pc.model)
