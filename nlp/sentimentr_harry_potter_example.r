library(devtools) 
install_github(‘trinker/sentimentr’) 
library(sentimentr)
library(dplyr) 
library(RColorBrewer) 
library(wordcloud)

devtools::install_github('bradleyboehmke/harrypotter') 

harry<-harrypotter::chamber_of_secrets

#make a density plot of the sentiments present in text 
makeDensityPlot <- function(txt) 
{
  txt %>%
    get_sentences() %>%
    sentiment() %>%
    filter(sentiment!=0) -> senti
  
  densitySentiments <- density(senti$sentiment) 
  
  plot(densitySentiments,main='Density of sentiments') 
  polygon(densitySentiments,col='red')        
  
  return(densitySentiments) 
}

makeDensityPlot(harry)


#for word clouds 
#set pos to FALSE if negative sentiment words are to be displayed 
makeWordCloud <- function(txt,pos=TRUE) 
{
  terms = extract_sentiment_terms(get_sentences(txt)) 
  #get words with positive or negative polarity depending upon the argument
  if (pos)
    attributes(terms)$counts %>% filter(polarity>0) -> wordSummary
  else
  {  attributes(terms)$counts %>% filter(polarity<0) -> wordSummary 
    #reverse the polarity for wordcloud
    wordSummary$polarity = wordSummary$polarity*-1
  }
  wordcloud(words=wordSummary$words,freq=wordSummary$polarity*100, 
            max.words=250,colors = brewer.pal(8, "Dark2"), scale = c(1, 0.1)) 
}

makeWordCloud(harry)

#plot the emotion_by object 
plotEmotionBy <- function(txt) 
{
  e<-emotion_by(get_sentences(txt),drop.unused.emotions=TRUE)
  plot(e) 
}

plotEmotionBy(harry) 

#make a barplot of different emotions 
plotEmotions <- function(rawTxt) 
{
  rawTxt %>%
    get_sentences() %>%
    emotion_by(drop.unused.emotions=TRUE) %>%
    group_by(emotion_type)  %>%
    summarise(ave_emotion=mean(ave_emotion)) -> txtSummary
  
  par(mar=c(11,4,4,4))
  barplot(txtSummary$ave_emotion,
          names.arg = txtSummary$emotion_type, las=2, col='red')
  return(txtSummary) 
}

plotEmotions(harry)
