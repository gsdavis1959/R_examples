library(rvest)
library(xml2)
library(stringr)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(caTools)
library(randomForest)

# load files

speeches <- lapply(paste0('https://www.presidency.ucsb.edu/documents/the-presidents-weekly-address-', 151:441), function(url){
  url %>% read_html() %>%
    html_nodes(".field-docs-content") %>%
    html_text()
})
name <- lapply(paste0('https://www.presidency.ucsb.edu/documents/the-presidents-weekly-address-', 151:441), function(url){
  url %>% read_html() %>%
    html_nodes(".diet-title") %>%
    html_text()
})
date <- lapply(paste0('https://www.presidency.ucsb.edu/documents/the-presidents-weekly-address-', 151:441), function(url){
  url %>% read_html() %>%
    html_nodes(".date-display-single") %>%
    html_text()
})

# combine lists into data frame
speech_data <- do.call(rbind, Map(data.frame, date=date, name=name, speech=speeches))
# split data into Trump and Obama
obama_speeches <- subset(speech_data, speech_data$name == 'Barack Obama')
trump_speeches <- subset(speech_data, speech_data$name == 'Donald J. Trump')

obama_corpus <- VCorpus(VectorSource(obama_speeches$speech))
trump_corpus <- VCorpus(VectorSource(trump_speeches$speech))

obama_corpus <- tm_map(obama_corpus, content_transformer(tolower))
obama_corpus <- tm_map(obama_corpus, removeNumbers)
obama_corpus <- tm_map(obama_corpus, removePunctuation)
obama_corpus <- tm_map(obama_corpus, removeWords, stopwords())
obama_corpus <- tm_map(obama_corpus, stemDocument)
obama_corpus <- tm_map(obama_corpus, stripWhitespace)

trump_corpus <- tm_map(trump_corpus, content_transformer(tolower))
trump_corpus <- tm_map(trump_corpus, removeNumbers)
trump_corpus <- tm_map(trump_corpus, removePunctuation)
trump_corpus <- tm_map(trump_corpus, removeWords, stopwords())
trump_corpus <- tm_map(trump_corpus, stemDocument)
trump_corpus <- tm_map(trump_corpus, stripWhitespace)

otdm <- TermDocumentMatrix(obama_corpus)
ttdm <- TermDocumentMatrix(trump_corpus)

# building Obama's data frame
om <- as.matrix(otdm)
ov <- sort(rowSums(om),decreasing=TRUE)
od <- data.frame(word = names(ov),freq=ov)
# generating Obama's word cloud
set.seed(1234)
wordcloud(words = od$word, freq = od$freq, min.freq = 1,     
          max.words=200, random.order=FALSE, rot.per=0.3,
          colors="blue")
# building Trump's data frame
tm <- as.matrix(ttdm)
tv <- sort(rowSums(tm),decreasing=TRUE)
td <- data.frame(word = names(tv),freq=tv)
# generating Trump's word cloud
set.seed(1234)
wordcloud(words = td$word, freq = td$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.3,
          colors='red')

# Trump's word associations
findAssocs(ttdm, "american", corlimit = 0.5)

# Obama's word associations
findAssocs(otdm, "american", corlimit = 0.25)

# both presidents
corpus = VCorpus(VectorSource(speech_data$speech))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.9)

data <- as.data.frame(as.matrix(dtm))
data$name <- speech_data$name

head(data)

# split sets
set.seed(1234)
split <- sample.split(data$name, SplitRatio = 0.75)
training_set <- subset(data, split == TRUE)
test_set <- subset(data, split == FALSE)


classifier <- randomForest(x = training_set[-470], 
                           y = training_set$name,
                           nTree = 10)

classifier

importance(classifier)
varImpPlot(classifier)

y_pred <- predict(classifier, newdata = test_set[-470])

cm <- table(test_set[, 470], y_pred)
cm

