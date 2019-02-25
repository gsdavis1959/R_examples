rm(list = ls(all = TRUE))
library(twitteR)
library(ggplot2)
library(XML)
library(doBy)
library(tm)
library(SnowballC)
library(syuzhet)
library(dplyr)
library(reshape)

setwd("~/Data/RStatistics/Datasets/")

consumer_key <- 'DAuDRmCqFxyWIOfIXOOedJC3Z'
consumer_secret <- 'PvL65HPCmXXq2si0TVOTJ97z0H6QWUUKWCOfHDfsDfHeetHcO9'
access_token <- '11455702-y9OZFSimskEtc44sVFnbNJAwCSm9yppPHw2KipCxw'
access_secret <- 'nADq6VKF1dDiUbUzDCs9f6O5RnkZivij38ThtZjNxkv0K'

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tweets <- userTimeline("realDonaldTrump", n=200)

n.tweet <- length(tweets)
tweets.df <- twListToDF(tweets)
head(tweets.df)
head(tweets.df$text)

tweets.df2 <- gsub("http.*","",tweets.df$text)
tweets.df2 <- gsub("https.*","",tweets.df2)
tweets.df2 <- gsub("#.*","",tweets.df2)
tweets.df2 <- gsub("@.*","",tweets.df2)
head(tweets.df2)

word.df <- as.vector(tweets.df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(tweets.df2, emotion.df) 

newdata <- subset(emotion.df2, select=anger:trust)
newdata
newdata %>% summarise_each(funs(sum))
qplot(newdata)

mdf <- melt(emotion.df2, id=emotion.df2$tweets.df2)
qplot(variable, value, data = mdf, geom = c("boxplot"))

# Simple Bar Plot 
counts <- table(mdf$value)
counts
barplot(counts, main="Trump Emotion Tweets",
        xlab="Emotion") 


write.csv(emotion.df2, file = "emotion.csv")
write.csv(mdf, file = "mdf.csv")


# sentiment
sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.positive
most.negative <- word.df[sent.value <= min(sent.value)] 
most.negative
View(sent.value)

positive.tweets <- word.df[sent.value > 0]
head(positive.tweets)
neutral.tweets <- word.df[sent.value == 0]
head(neutral.tweets)
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
head(category_senti)

category_senti2 <- cbind(tweets.df2, category_senti)
View(category_senti2)
head(category_senti2)
table(category_senti)

