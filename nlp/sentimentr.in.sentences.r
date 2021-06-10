library(tidyverse)
library(sentimentr)
library(htmltab)

text <- read.csv('C:/Users/gsdav/Documents/Data/Datasets/Text/Messages - Nancy Davis.csv', stringsAsFactors = FALSE) %>% glimpse()

text %>% 
  sentimentr::get_sentences() %>% 
  sentimentr::sentiment() %>% 
  mutate(characters = (Text)) %>% 
  filter(characters >1 ) -> bounded_sentences 
summary(bounded_sentences$sentiment)

print(bounded_sentences$sentiment)
