library(syuzhet)
library(remotes)
library(RCurl) 
library(tidyverse)


url <- "sftp://raspberrypi/home/pi/MarSec.csv"
text_data <- getURL(url, userpwd = "pi:clayclaire", connecttimeout = 60)
df <- read.csv(text = text_data, col.names = c('date', 'text', 'lang', 'n'), stringsAsFactors = FALSE)
head(df$text)
names(df)
df <- df %>% filter(lang == 'en')
df <- df[1:100,]

# Converting tweets to ASCII to trackle strange characters
tweets <- iconv(df$text, from="UTF-8", to="ASCII", sub="")
# removing retweets, in case needed 
tweets <-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tweets)
# removing mentions, in case needed
tweets <-gsub("@\\w+","",tweets)
ew_sentiment<-get_nrc_sentiment((tweets))
ew_sentiment
sentimentscores<-data.frame(colSums(ew_sentiment[,]))
names(sentimentscores) <- "Score"
sentimentscores <- cbind("sentiment"=rownames(sentimentscores),sentimentscores)
rownames(sentimentscores) <- NULL

library(plotly)
p <- ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+
  geom_bar(aes(fill=sentiment),stat = "identity")+
  theme(legend.position="none")+
  xlab("Sentiments")+ylab("Scores")+
  ggtitle("Total sentiment based on scores")+
  theme_minimal()
ggplotly(p)
