library(udpipe)
en <- udpipe::udpipe_download_model("english")
library(itunesr)
library(tidyverse)

reviews1 <- getReviews("297606951", "us", 1)

reviews2 <- getReviews("297606951", "us", 2)

reviews <- rbind(reviews1, reviews2)

head(reviews)

reviews_neg <- reviews[reviews$Rating %in% c('1','2'),]

nrow(reviews_neg)
model <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

doc <- udpipe::udpipe_annotate(model, reviews_neg$Review)
names(as.data.frame(doc))

doc_df <- as.data.frame(doc)

topics <- keywords_rake(x = doc_df, term = "lemma", group = "doc_id", 
                        relevant = doc_df$upos %in% c("NOUN", "ADJ"))

head(topics)

topics %>% 
  head() %>% 
  ggplot() + geom_bar(aes(x = keyword,
                          y = rake), stat = "identity",
                      fill = "#ff2211") +
  
  theme_minimal() +
  labs(title = "Top Topics of Negative Customer Reviews",
       subtitle = "Amazon US iOS App",
       caption = "Apple App Store")
