library(tidyverse)
library(wordcloud)
library(gutenbergr)
library(tidytext)
library(tidyr)
library(ggraph)
library(igraph)
library(reshape2)

twain_books <- gutenberg_download(c( 74, 76,3177,3178), meta_fields = "title")

tidy_books <- twain_books %>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>%
  top_n(10)%>%
  ggplot(aes(word, n,fill=-n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  ggtitle("The Most Common Words in Mark Twains' Novels")


books_bigrams <- twain_books %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
books_bigrams

bigrams_separated <- books_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_filtered

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united



# most common words

bigram_tf_idf <- bigrams_united %>%
  count(title, bigram) %>%
  bind_tf_idf(bigram, title, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(title) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ title, ncol = 2, scales = "free") +
  coord_flip() +
  labs(y = "tf-idf of bigram to novel",
       x = "")

# graph counts
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

set.seed(2020)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  ggtitle("Network of Bigrams in Mark Twains' Novels")


twain_books %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# sentiment
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

section_words <- twain_books %>%
  mutate(section = row_number()) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
mark_twain_sentiment <- section_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, index = section %/% 80, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

ggplot(mark_twain_sentiment, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")


# Review tail of all_books
books_sent_count <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(title, sentiment, sort = TRUE) %>%
  ungroup()
book_pos <- books_sent_count %>%
  # Group by book
  group_by(title) %>% 
  # Mutate to add % positive column 
  mutate(percent_positive = 100 * n / sum(n) )
# Plot percent_positive vs. book, filled by sentiment
ggplot(book_pos, aes(title,percent_positive, fill = sentiment)) +  
  # Add a col layer
  geom_col()+ 
  ggtitle("Proportion of Positive/Negative Sentiment Scores")


negation_words <- c("not", "no","can't","don't")
negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)
negated_words %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  top_n(10, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free",ncol=2) +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurrences") +
  coord_flip()+ 
  ggtitle("Positive/Negative Words that Follow Negations")


# This dataset was published in Saif M. Mohammad and Peter Turney. (2013), ``Crowdsourcing a Word-Emotion Association Lexicon.'' Computational Intelligence, 29(3): 436-465.
#article{mohammad13,
#  author = {Mohammad, Saif M. and Turney, Peter D.},
#  title = {Crowdsourcing a Word-Emotion Association Lexicon},
#  journal = {Computational Intelligence},
#  volume = {29},
#  number = {3},
#  pages = {436-465},
#  doi = {10.1111/j.1467-8640.2012.00460.x},
#  url = {https://onlinelibrary.wiley.com/doi/abs/10.1111/j.1467-8640.2012.00460.x},
#  eprint = {https://onlinelibrary.wiley.com/doi/pdf/10.1111/j.1467-8640.2012.00460.x},
#  year = {2013}


tidy_books %>%
  count(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  group_by(sentiment)%>%
  filter(!grepl("positive|negative", sentiment)) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(words = reorder(word, n)) %>%
  ggplot(aes(x = word, fill = sentiment)) +
  facet_grid(~sentiment) +
  geom_bar() + #Create a bar for each word per sentiment
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_blank()) + #Place the words on the y-axis
  xlab(NULL) + ylab(NULL) +
  ggtitle("Words From Mark Twains' Novels") +
  coord_flip()+ 
  theme(legend.position="none")

# Review tail of all_books
books_sent_count2 <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!grepl("positive|negative", sentiment)) %>%
  count(sentiment, sort = TRUE) %>%
  ungroup()
books_sent_count2%>%
  ggplot( aes(x=reorder(sentiment,n), y=n,fill=sentiment)) +
  geom_col()+
  coord_flip() + 
  theme(legend.position="none")+ 
  ggtitle("Top Sentiments in Mark Twains' Novels using NRC Lexicon")


scores <- tidy_books%>%
  # filter(title != "Roughing It")%>%
  # filter(title != "Adventures of Huckleberry Finn")%>%
  inner_join(get_sentiments("nrc"), by = c("word" = "word")) %>% 
  filter(!grepl("positive|negative", sentiment)) %>% 
  count(title, sentiment) 
scores%>%
  group_by(title)%>%
  mutate(total = (n/sum(n))*100) %>%
  ggplot( aes(x=sentiment, y=total,fill=sentiment)) +
  geom_bar(stat="identity", width=0.6) +
  coord_flip() +
  facet_wrap(~title, ncol=4)+
  theme(legend.position="none")+ 
  ggtitle("Comparison of Sentiments from Each of the Novels")
