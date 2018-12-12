setwd("~/Data/RStatistics/Datasets")
df = read.csv("unigram_wiki.csv", header=TRUE)
require(wordcloud)
View(df)

newdata <- subset(df, count > 2000)
View(newdata)
wordcloud(newdata$word,newdata$count)
