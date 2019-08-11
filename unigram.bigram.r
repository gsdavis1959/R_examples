library(itunesr)
library(tidyverse)
library(tidytext)

# Extracting Medium iOS App Reviews
medium <- getReviews("828256236","us",1)
head(medium)
table(medium$Rating)

reviews <- data.frame(txt = medium$Review[medium$Rating==5],
                      stringsAsFactors = FALSE)
reviews %>% 
  unnest_tokens(output = word, input = txt) %>% 
  head()
reviews %>% 
  unnest_tokens(output = word, input = txt) %>% 
  count(word, sort = TRUE) 
reviews %>% 
  unnest_tokens(output = word, input = txt) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) 

# plot
reviews %>% 
  unnest_tokens(output = word, input = txt) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  labs(title = "Top unigrams of Medium iOS App Reviews",
       subtitle = "using Tidytext in R",
       caption = "Data Source: itunesr - iTunes App Store")
# bigrams
reviews %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams of Medium iOS App Reviews",
       subtitle = "using Tidytext in R",
       caption = "Data Source: itunesr - iTunes App Store")

# trigrams
reviews %>% 
  unnest_tokens(word, txt, token = "ngrams", n = 3) %>% 
  separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  unite(word,word1, word2, word3, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:20) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Trigrams of Medium iOS App Reviews",
       subtitle = "using Tidytext in R",
       caption = "Data Source: itunesr - iTunes App Store")
