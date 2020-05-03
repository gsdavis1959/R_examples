library(tidyverse)
library(remotes)
library(RCurl) 
library(textclean)
library(tm)
library(qdapRegex)
library(tidytext)
url <- "sftp://raspberrypi/home/pi/MarSec.csv"
text_data <- getURL(url, userpwd = "pi:clayclaire", connecttimeout = 60)
df <- read.csv(text = text_data, col.names = c('date', 'text', 'lang', 'n'), stringsAsFactors = FALSE)
head(df)
names(df)
df <- df %>% filter(lang == 'en')
check_text(df$text)
df$text <- strip(df$text)
df$text <- replace_contraction(df$text)
df$text <- replace_emoji(df$text)
df$text <- replace_emoticons(df$text)
df$text <- replace_html(df$text)
df$text <- replace_hash(df$text)
df$text <- replace_incomplete(df$text)
df$text <- replace_internet_slang(df$text)
df$text <- replace_kern(df$text)
df$text <- replace_url(df$text)
df$text <- add_missing_endmark(df$text)

write.csv(df, 'tweets.csv')

tweets <- read.csv('tweets.csv', stringsAsFactors = FALSE)
head(tweets$text)
tweets$text <- replace_names(tweets$text)
tweets$text <- gsub("http.*","",tweets$text)
tweets$text <- gsub("https.*","",tweets$text)
tweets$text <- gsub("#.*","",tweets$text)
tweets$text <- gsub("@.*","",tweets$text)
tweets$text <- gsub("b'rt","",tweets$text)
tweets$text <- gsub("xe","",tweets$text)
tweets$text <- gsub("x","",tweets$text)
tweets$text <- gsub("fa","",tweets$text)
tweets$text <- gsub("fb","",tweets$text)
tweets$text <- gsub("f","",tweets$text)
tweets$text <- gsub("s","",tweets$text)
tweets$text <- rm_non_words(tweets$text)
tweets$text <- rm_repeated_words(tweets$text)
head(tweets)

documents <- data.frame(text = tweets$text)
documents <- rowid_to_column(documents, var = "doc_id")

corpus <- Corpus(DataframeSource(documents))
corpus <- tm_map(corpus, PlainTextDocument)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, content_transformer(tolower))

dtm <- DocumentTermMatrix(corpus)
tdm <- TermDocumentMatrix(corpus)
dtm
dtm2 <- removeSparseTerms(dtm, sparse=0.70)
inspect(dtm2)
inspect(tdm)



df_dtm <- tidy(dtm)
head(df_dtm)
documents$doc_id <- as.character(documents$doc_id)
tweets <- rowid_to_column(tweets, var = "document")
tweets$document <- as.character(tweets$document)
w_rt <- left_join(df_dtm, tweets, by = 'document')
head(w_rt)


word_group <- w_rt %>% group_by(term, count) %>%
  summarize(total = sum(n)) %>% 
  ungroup() %>% 
  arrange(-total) %>% 
  print()
dim(word_group)

require(psych)
require(corrplot)
corMat <- cor(select_if(word_group, is.numeric))
corrplot(corMat)
