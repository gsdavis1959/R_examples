library(tidyverse)   # For ggplot, dplyr, etc.
library(tidytext)    # For neat text things
library(gutenbergr)  # For downloading books from Project Gutenberg

little_women_raw <- gutenberg_download(514, meta_fields = "title")
head(little_women_raw)
tragedies_raw <- gutenberg_download(c(1524, 1532, 1533, 1513),
                                    meta_fields = "title")

# Clean up Little Women
little_women <- little_women_raw %>% 
  # The actual book doesn't start until line 70
  slice(70:n()) %>% 
  # Get rid of rows where text is missing
  drop_na(text) %>% 
  # Chapters start with CHAPTER X, so mark if each row is a chapter start
  # cumsum() calculates the cumulative sum, so it'll increase every time there's
  # a new chapter and automatically make chapter numbers
  mutate(chapter_start = str_detect(text, "^CHAPTER"),
         chapter_number = cumsum(chapter_start)) %>% 
  # Get rid of these columns
  select(-gutenberg_id, -title, -chapter_start)

head(little_women)

tragedies_words <- tragedies_raw %>% 
  drop_na(text) %>% 
  unnest_tokens(word, text)

head(tragedies_words)

top_words_tragedies <- tragedies_words %>% 
  # Remove stop words
  anti_join(stop_words) %>% 
  # Get rid of old timey words and stage directions
  filter(!(word %in% c("thou", "thy", "haue", "thee", 
                       "thine", "enter", "exeunt", "exit"))) %>% 
  # Count all the words in each play
  count(title, word, sort = TRUE) %>% 
  # Keep top 15 in each play
  group_by(title) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  # Make the words an ordered factor so they plot in order
  mutate(word = fct_inorder(word))
top_words_tragedies

ggplot(top_words_tragedies, aes(y = fct_rev(word), x = n, fill = title)) + 
  geom_col() + 
  guides(fill = FALSE) +
  labs(y = "Count", x = NULL, 
       title = "15 most frequent words in four Shakespearean tragedies") +
  facet_wrap(vars(title), scales = "free_y") +
  theme_bw()

tragedies_bigrams <- tragedies_raw %>% 
  drop_na(text) %>% 
  # n = 2 here means bigrams. We could also make trigrams (n = 3) or any type of n-gram
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  # Split the bigrams into two words so we can remove stopwords
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  filter(!word1 %in% c("thou", "thy", "thine", "enter", "exeunt", "exit"),
         !word2 %in% c("thou", "thy", "thine", "enter", "exeunt", "exit")) %>% 
  # Put the two word columns back together
  unite(bigram, word1, word2, sep = " ")
tragedies_bigrams

top_bigrams <- tragedies_bigrams %>% 
  # Count all the bigrams in each play
  count(title, bigram, sort = TRUE) %>% 
  # Keep top 15 in each play
  group_by(title) %>% 
  top_n(15) %>% 
  ungroup() %>% 
  # Make the bigrams an ordered factor so they plot in order
  mutate(bigram = fct_inorder(bigram))

ggplot(top_bigrams, aes(y = fct_rev(bigram), x = n, fill = title)) + 
  geom_col() + 
  guides(fill = FALSE) +
  labs(y = "Count", x = NULL, 
       title = "15 most frequent bigrams in four Shakespearean tragedies") +
  facet_wrap(vars(title), scales = "free") +
  theme_bw()

pronouns <- c("he", "she")

bigram_he_she_counts <- tragedies_raw %>%
  drop_na(text) %>% 
  # Split into bigrams
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  # Find counts of bigrams
  count(bigram, sort = TRUE) %>%
  # Split the bigram column into two columns
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # Only choose rows where the first word is he or she
  filter(word1 %in% pronouns) %>%
  count(word1, word2, wt = n, sort = TRUE) %>% 
  rename(total = n)

word_ratios <- bigram_he_she_counts %>%
  # Look at each of the second words
  group_by(word2) %>%
  # Only choose rows where the second word appears more than 10 times
  filter(sum(total) > 10) %>%
  ungroup() %>%
  # Spread out the word1 column so that there's a column named "he" and one named "she"
  spread(word1, total, fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the she counts to he counts
  mutate(logratio = log2(she / he)) %>%
  # Sort by that ratio
  arrange(desc(logratio))

# Rearrange this data so it's plottable
plot_word_ratios <- word_ratios %>%
  # This gets the words in the right order---we take the absolute value, select
  # only rows where the log ratio is bigger than 0, and then take the top 15 words
  mutate(abslogratio = abs(logratio)) %>%
  group_by(logratio < 0) %>%
  top_n(15, abslogratio) %>%
  ungroup() %>%
  mutate(word = reorder(word2, logratio)) 

# Finally we plot this
ggplot(plot_word_ratios, aes(y = word, x = logratio, color = logratio < 0)) +
  geom_segment(aes(y = word, yend = word,
                   x = 0, xend = logratio), 
               size = 1.1, alpha = 0.6) +
  geom_point(size = 3.5) +
  labs(x = "How much more/less likely", y = NULL) +
  scale_color_discrete(name = "", labels = c("More 'she'", "More 'he'")) +
  scale_x_continuous(breaks = seq(-3, 3),
                     labels = c("8x", "4x", "2x",
                                "Same", "2x", "4x", "8x")) +
  theme_bw() +
  theme(legend.position = "bottom")

tragedy_words <- tragedies_raw %>% 
  drop_na() %>% 
  # Split into word tokens
  unnest_tokens(word, text) %>% 
  # Remove stop words and old timey words
  anti_join(stop_words) %>% 
  filter(!word %in% c("thou", "thy", "haue", "thee", 
                      "thine", "enter", "exeunt", "exit")) %>% 
  count(title, word, sort = TRUE)

# Add the tf-idf values to the counts
tragedy_tf_idf <- tragedy_words %>% 
  bind_tf_idf(word, title, n)

# Get the top 10 uniquest words
tragedy_tf_idf_plot <- tragedy_tf_idf %>% 
  arrange(desc(tf_idf)) %>% 
  group_by(title) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = fct_inorder(word))

ggplot(tragedy_tf_idf_plot, 
       aes(y = fct_rev(word), x = tf_idf, fill = title)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~ title, scales = "free") +
  theme_bw()

get_sentiments("afinn")  # Scoring system

# get_sentiments("bing")  # Negative/positive
# get_sentiments("nrc")  # Specific emotions
# get_sentiments("loughran")  # Designed for financial statements; positive/negative

tragedy_words <- tragedies_raw %>% 
  drop_na() %>% 
  # Split into word tokens
  unnest_tokens(word, text) %>% 
  # Remove stop words and old timey words
  anti_join(stop_words) %>% 
  filter(!word %in% c("thou", "thy", "haue", "thee", 
                      "thine", "enter", "exeunt", "exit"))

# Join the sentiment dictionary 
tragedy_sentiment <- tragedy_words %>% 
  inner_join(get_sentiments("bing"))
tragedy_sentiment

tragedy_sentiment_plot <- tragedy_sentiment %>% 
  count(title, sentiment)

ggplot(tragedy_sentiment_plot, aes(x = sentiment, y = n, fill = title, alpha = sentiment)) +
  geom_col(position = position_dodge()) +
  scale_alpha_manual(values = c(0.5, 1)) +
  facet_wrap(vars(title)) +
  theme_bw()

tragedies_split_into_lines <- tragedy_sentiment %>% 
  # Divide lines into groups of 100
  mutate(line = row_number(),
         line_chunk = line %/% 100) %>% 
  # Get a count of postiive and negative words in each 100-line chunk in each play
  count(title, line_chunk, sentiment) %>% 
  # Convert the sentiment column into two columns named "positive" and "negative"
  pivot_wider(names_from = sentiment, values_from = n) %>% 
  # Calculate net sentiment
  mutate(sentiment = positive - negative)

ggplot(tragedies_split_into_lines,
       aes(x = line_chunk, y = sentiment, fill = sentiment)) +
  geom_col() +
  scale_fill_viridis_c(option = "magma", end = 0.9) +
  facet_wrap(vars(title), scales = "free_x") +
  theme_bw()

# For the tagger to work, each row needs to be unique, which means we need to
# combine all the text into individual chapter-based rows. This takes a little
# bit of text-wrangling with dplyr:
little_women_to_tag <- little_women %>% 
  # Group by chapter number
  group_by(chapter_number) %>% 
  # Take all the rows in each chapter and collapse them into a single cell
  nest(data = c(text)) %>% 
  ungroup() %>% 
  # Look at each individual cell full of text lines and paste them together into
  # one really long string of text per chapter
  mutate(text = map_chr(data, ~paste(.$text, collapse = " "))) %>% 
  # Get rid of this column
  select(-data)
little_women_to_tag

library(cleanNLP)

# Use the built-in R-based tagger
cnlp_init_udpipe()

little_women_tagged <- cnlp_annotate(little_women_to_tag, 
                                     text_name = "text", 
                                     doc_name = "chapter_number")
little_women_tagged <- as.data.frame(little_women_tagged)
str(little_women_tagged)
# Find all proper nouns
proper_nouns <- little_women_tagged$token %>% 
  filter(upos == "PROPN")

main_characters_by_chapter <- proper_nouns %>% 
  # Find only Meg, Jo, Beth, and Amy
  filter(lemma %in% c("Meg", "Jo", "Beth", "Amy")) %>% 
  # Group by chapter and character name
  group_by(doc_id, lemma) %>% 
  # Get the count of mentions
  summarize(n = n()) %>% 
  # Make a new column named "name" that is an ordered factor of the girls' names
  mutate(name = factor(lemma, levels = c("Meg", "Jo", "Beth", "Amy"), ordered = TRUE)) %>% 
  # Rename this so it's called chapter
  rename(chapter = doc_id) %>% 
  # Group by chapter
  group_by(chapter) %>% 
  # Calculate the proportion of each girl's mentions in each chapter
  mutate(prop = n / sum(n)) %>% 
  ungroup() %>% 
  # Make a cleaner chapter name column
  mutate(chapter_name = paste("Chapter", chapter)) %>% 
  mutate(chapter_name = fct_inorder(chapter_name))
main_characters_by_chapter

ggplot(main_characters_by_chapter, aes(x = prop, y = 1, fill = fct_rev(name))) + 
  geom_col(position = position_stack()) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(option = "plasma", end = 0.9, name = NULL) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = NULL, y = NULL,
       title = "Proportion of mentions of each\nLittle Woman per chapter",
       subtitle = "Jo basically dominates the last third of the book") +
  facet_wrap(vars(chapter_name), nrow = 6) +
  theme_bw(base_family = "Roboto Condensed") +
  theme(legend.position = "top",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_rect(fill = "white"),
        legend.text = element_text(face = "bold", size = rel(1)),
        plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1.7)),
        plot.subtitle = element_text(hjust = 0.5, size = rel(1.1)))
