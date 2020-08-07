library(sentimentr)
library(tidyverse)

text <- 'My life has become terrible since I met you and lost money'

#aggregated sentiment analysis

sentiment_by(text, by = NULL)

#sentence-level sentiment analysis

sentiment(text)

#extracting sentiment terms

text %>% extract_sentiment_terms()


install.packages("ndjson")
library(ndjson)
df = stream_in("Toys_and_Games_5.json")
head(df)

df1 <- df[1:500,]

sentiment=sentiment_by(df1$reviewText)

summary(sentiment$ave_sentiment)

library(ggplot2)
qplot(sentiment$ave_sentiment,   geom="histogram",binwidth=0.1,main="Review Sentiment Histogram")

df1$ave_sentiment=sentiment$ave_sentiment
df1$sd_sentiment=sentiment$sd
terms <- df1$reviewText %>% extract_sentiment_terms()
glimpse(terms)

