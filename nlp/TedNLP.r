library(jsonlite)

set.seed(5152)
ted_main <- read.csv('ted_main.csv')
transcripts <- read.csv('transcripts.csv')
ted_talks <- merge(x = ted_main,y = transcripts, by = 'url')

library(tm)
library(SnowballC)
corpus <- VCorpus(VectorSource(ted_talks$transcript))
##Removing Punctuation
corpus <- tm_map(corpus, content_transformer(removePunctuation))
##Removing numbers
corpus <- tm_map(corpus, removeNumbers)
##Converting to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
##Removing stop words
corpus <- tm_map(corpus, content_transformer(removeWords), stopwords('english'))
##Stemming
corpus <- tm_map(corpus, stemDocument)
##Whitespace
corpus <- tm_map(corpus, stripWhitespace)

# Create Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
# Removing all terms whose sparsity is greater than 95% 
corpus <- removeSparseTerms(dtm, 0.95)

library(data.table)
library(ggplot2)
library(ggthemes)
colS <- colSums(as.matrix(corpus))
doc_features <- data.table(name = attributes(colS)$names, count = colS)
ggplot(doc_features[count>8500],aes(name, count)) + 
geom_bar(stat = 'identity',fill='lightblue',color='black')+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ 
  theme_economist()+ scale_color_economist()

install.packages("tm.lexicon.GeneralInquirer", repos="http://datacube.wu.ac.at", type="source")
library(tm.lexicon.GeneralInquirer)
install.packages("tm.plugin.sentiment", repos="http://R-Forge.R-project.org")
library(tm.plugin.sentiment)


words_with_emotions <- terms_in_General_Inquirer_categories('EMOT')
words_with_pleasur <- terms_in_General_Inquirer_categories('Pleasur')
words_with_pain <- terms_in_General_Inquirer_categories('Pain')
words_with_virtue <- terms_in_General_Inquirer_categories('Virtue')
words_with_vice <- terms_in_General_Inquirer_categories('Vice')

emo_score <- tm_term_score(dtm, words_with_emotions)
pleasure_score <- tm_term_score(dtm, words_with_pleasur)
pain_score <- tm_term_score(dtm, words_with_pain)
virtue_score <- tm_term_score(dtm, words_with_virtue)
vice_score <- tm_term_score(dtm, words_with_vice)

library(dplyr)
library(tibble)
library(tidyr)
library(tidytext)
library(gplots)
doc_emo_df <- tbl_df(emo_score) %>% rownames_to_column %>% rename(document = rowname) %>% rename(emotional = value)
doc_pleasure_df <- tbl_df(pleasure_score) %>% rownames_to_column %>% rename(document = rowname) %>% rename(pleasure = value)
doc_pain_df <- tbl_df(pain_score) %>% rownames_to_column %>% rename(document = rowname) %>% rename(pain = value)
doc_virtue_df <- tbl_df(virtue_score) %>% rownames_to_column %>% rename(document = rowname) %>% rename(virtue = value)
doc_vice_df <- tbl_df(vice_score) %>% rownames_to_column %>% rename(document = rowname) %>% rename(vice = value)

library(tidyverse)
all_emotions <- list(doc_emo_df, doc_pleasure_df, doc_pain_df, doc_virtue_df, doc_vice_df) %>% reduce(inner_join, by = 'document') %>% print()

# transform the columns (except document number) into a matrix
mat_data <- data.matrix(all_emotions[,2:ncol(all_emotions)]) 
rownames(mat_data) <- all_emotions[,1][[1]] # assign row names
heatmap.2(mat_data,
          cellnote = mat_data, # same data set for cell labels
          main = 'Emotions in Ted Talks', # heat map title
          notecol='black', # change font color of cell labels to black
          density.info='none', # turns off density plot inside color legend
          trace='none', # turns off trace lines inside the heat map
          margins =c(12,9), # widens margins around plot
          col=colorRampPalette(c('red', 'yellow', 'green'))(n = 299), # use on color palette defined earlier
          Colv='NA', # turn off column clustering
          dendrogram='none')

library(jsonlite)
formatted_ted_ratings <- gsub("'",'"',ted_talks$ratings)

ted_ratings <- purrr::map(formatted_ted_ratings, jsonlite::fromJSON) %>% print()

for (i in (1:length(ted_ratings))) {
  ted_ratings_df <- ted_ratings[[i]]
  highest_rating_count <- ted_ratings_df[which(ted_ratings_df$count == max(ted_ratings_df$count)), ]
  ted_talks$highest_rating[i] <- highest_rating_count$name
}
ted_talks$highest_rating = as.factor(ted_talks$highest_rating)

trainObs <- sample(nrow(ted_talks), .6 * nrow(ted_talks), replace = FALSE)
testObs <- sample(nrow(ted_talks), .4 * nrow(ted_talks), replace = FALSE)
train_dat <- ted_talks[trainObs,]
test_dat <- ted_talks[testObs,]

train_corpus <- VCorpus(VectorSource(train_dat$transcript))
##Removing Punctuation
train_corpus <- tm_map(train_corpus, content_transformer(removePunctuation))
##Removing numbers
train_corpus <- tm_map(train_corpus, removeNumbers)
##Converting to lower case
train_corpus <- tm_map(train_corpus, content_transformer(tolower))
##Removing stop words
train_corpus <- tm_map(train_corpus, content_transformer(removeWords), stopwords('english'))
##Stemming
train_corpus <- tm_map(train_corpus, stemDocument)
##Whitespace
train_corpus <- tm_map(train_corpus, stripWhitespace)
# Create Document Term Matrix
dtm_train <- DocumentTermMatrix(train_corpus)
train_corpus <- removeSparseTerms(dtm_train, 0.4)
dtm_train_matrix <- as.matrix(train_corpus)
dtm_train_matrix <- cbind(dtm_train_matrix, train_dat$highest_rating)
colnames(dtm_train_matrix)[ncol(dtm_train_matrix)] <- 'y'
training_set_ted_talk <- as.data.frame(dtm_train_matrix)
training_set_ted_talk$y <- as.factor(training_set_ted_talk$y)

library(caret)
review_ted_model <- train(y ~., data = training_set_ted_talk, method = 'svmLinear3')
# Preparing our test data. It’s the same repetitive procedure.
test_corpus <- VCorpus(VectorSource(test_dat$transcript))
##Removing Punctuation
test_corpus <- tm_map(test_corpus, content_transformer(removePunctuation))
##Removing numbers
test_corpus <- tm_map(test_corpus, removeNumbers)
##Converting to lower case
test_corpus <- tm_map(test_corpus, content_transformer(tolower))
##Removing stop words
test_corpus <- tm_map(test_corpus, content_transformer(removeWords), stopwords('english'))
##Stemming
test_corpus <- tm_map(test_corpus, stemDocument)
##Whitespace
test_corpus <- tm_map(test_corpus, stripWhitespace)
# Create Document Term Matrix
dtm_test <- DocumentTermMatrix(test_corpus)
test_corpus <- removeSparseTerms(dtm_test, 0.4)
dtm_test_matrix <- as.matrix(test_corpus)

#Build the prediction 
model_ted_talk_result <- predict(review_ted_model, newdata = dtm_test_matrix)
check_accuracy <- as.data.frame(cbind(prediction = model_ted_talk_result, rating = test_dat$highest_rating))
library(dplyr)
check_accuracy <- check_accuracy %>% mutate(prediction = as.integer(prediction) - 1)
check_accuracy$accuracy <- if_else(check_accuracy$prediction == check_accuracy$rating, 1, 0)
round(prop.table(table(check_accuracy$accuracy)), 3)
library(performanceEstimation)
classificationMetrics(as.integer(test_dat$highest_rating), model_ted_talk_result)
most_common_misclassified_ratings = check_accuracy %>% filter(check_accuracy$accuracy == 0) %>%
  group_by(rating) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(3)
##Most commong missclassified rating
levels(train_dat$highest_rating)[most_common_misclassified_ratings$rating]

most_common_correct_ratings = check_accuracy %>% filter(check_accuracy$accuracy == 1) %>%
  group_by(rating) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10)

##Most commong correct rating
levels(train_dat$highest_rating)[most_common_correct_ratings$rating]
