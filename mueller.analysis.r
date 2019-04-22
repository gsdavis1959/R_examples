library(tidyverse)

mueller_report <- read_csv("mueller_report.csv")



# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# fix (expand) contractions
mueller_report$text <- sapply(mueller_report$text, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", "", x)
# remove special characters
mueller_report$text <- sapply(mueller_report$text, removeSpecialChars)

# convert everything to lower case
mueller_report$text <- sapply(mueller_report$text, tolower)

# stopwords


#get facts about the full dataset
summary(mueller_report)

my_colors <- c("#E69F00", "#56B4E9", "#009E73", "#CC79A7", "#D55E00")

theme_lyrics <- function() 
{
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")
}

# words
full_word_count <- mueller_report %>%
  group_by(page) %>%
  summarise(num_words = n()) %>%
  arrange(desc(num_words))




library(ggthemes)
ggplot(data = full_word_count) +
  aes(x = page, y = num_words) +
  geom_line(color = "#0c4c8a") +
  labs(title = "Number of Words per Page",
       x = "Page",
       y = "Count") +
  theme_economist()


library(wordcloud2)
library(tidytext)
library(dplyr)
tidy_mueller<- mueller_report %>%
  select(page,text) %>%
  unnest_tokens("word", text) 



tidy_mueller_count <- tidy_mueller %>%
  count(word) %>%
  arrange(desc(n))
  


tidy_mueller_count <- tidy_mueller_count %>%
  anti_join(stop_words, by= c("word" = "word")) %>%
  mutate(row = row_number())


tidy_mueller_count$word <- gsub("[0-9]+", "", tidy_mueller_count$word)
tidy_mueller_count$word[tidy_mueller_count$word==""]  <- NA 
tidy_mueller_count <- na.omit(tidy_mueller_count)
tidy_mueller_count

wordcloud2(tidy_mueller_count, size = 1)

library(ggthemes)
ggplot(data = tidy_mueller_count) +
  aes(x = word, y = n) +
  geom_line(color = "#0c4c8a") +
  coord_flip() +
  labs(title = "Number of Words",
       x = "Word",
       y = "Count") +
  theme_economist()


# sentiment analysis
library(tidyr)
library(widyr) #Use for pairwise correlation
library(kableExtra) #Create nicely formatted output tables
new_sentiments <- sentiments %>% #From the tidytext package
  filter(lexicon != "loughran") %>% #Remove the finance lexicon
  mutate( sentiment = ifelse(lexicon == "AFINN" & score >= 0, "positive",
                             ifelse(lexicon == "AFINN" & score < 0,
                                    "negative", sentiment))) %>%
  group_by(lexicon) %>%
  mutate(words_in_lexicon = n_distinct(word)) %>%
  ungroup()


mueller_sentiments <- left_join(tidy_mueller_count, new_sentiments, by="word") %>%
  glimpse()
all_sentiments <- mueller_sentiments %>%
  select(word, n, sentiment) %>%
  glimpse()
all_sentiments <- na.omit(all_sentiments)
mueller_sentiments <- na.omit(mueller_sentiments)
mueller_sentiments

positive_words <- mueller_sentiments %>%
  filter(score > 1)
positive_words

negative_words <- mueller_sentiments %>%
  filter(score < 0)
negative_words


# plot
ggplot(data = mueller_sentiments) +
  aes(x = sentiment, y = score) +
  geom_boxplot(fill = "#0c4c8a") +
  labs(title = "Mueller Report Sentiment",
       x = "Sentiment",
       y = "Score") +
  theme_economist()


ggplot(data = all_sentiments) +
  aes(x = sentiment) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "Mueller Report Sentiment",
       x = "Sentiment",
       y = "Count") +
  theme_economist()
