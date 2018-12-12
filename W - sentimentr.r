library(sentimentr)
mytext <- c(
  'do you like it?  But I hate really bad dogs',
  'I am the best friend.',
  "Do you really like it?  I'm not a fan",
  "It's like a tree."
)

## works on a character vector but not the preferred method avoiding the 
## repeated cost of doing sentence boundary disambiguation every time 
## `sentiment` is run.  For small batches the loss is minimal.
## Not run: 
sentiment(mytext)

## End(Not run)

## preferred method avoiding paying the cost 
mytext <- get_sentences(mytext)
sentiment(mytext)
sentiment(mytext, question.weight = 0)

sam_dat <- get_sentences(gsub("Sam-I-am", "Sam I am", sam_i_am))
(sam <- sentiment(sam_dat))
plot(sam)
plot(sam, scale_range = TRUE, low_pass_size = 5)
plot(sam, scale_range = TRUE, low_pass_size = 10)

plot(sam, transformation.function = syuzhet::get_transformed_values)
plot(sam, transformation.function = syuzhet::get_transformed_values,  
     scale_range = TRUE, low_pass_size = 5)

y <- get_sentences(
  "He was not the sort of man that one would describe as especially handsome."
)
sentiment(y)
sentiment(y, n.before=Inf)

dat <- data.frame(
  w = c('Person 1', 'Person 2'),
  x = c(paste0(
    "Mr. Brown is nasty! He says hello. i give him rage.  i will ",
    "go at 5 p. m. eastern time.  Angry thought in between!go there"
  ), "One more thought for the road! I am going now.  Good day and good riddance."),
  y = state.name[c(32, 38)], 
  z = c(.456, .124),
  stringsAsFactors = FALSE
)
sentiment(get_sentences(dat$x))
sentiment(get_sentences(dat))

## Not run: 
## tidy approach
library(dplyr)
library(magrittr)

hu_liu_cannon_reviews %>%
  mutate(review_split = get_sentences(text)) %$%
  sentiment(review_split)

## End(Not run)

## Emojis
## Not run: 
## Load R twitter data
x <- read.delim(system.file("docs/r_tweets.txt", package = "textclean"), 
                stringsAsFactors = FALSE)

x

library(dplyr); library(magrittr)

## There are 2 approaches
## Approach 1: Replace with words
x %>%
  mutate(Tweet = replace_emoji(Tweet)) %$%
  sentiment(Tweet)

## Approach 2: Replace with identifier token
combined_emoji <- update_polarity_table(
  lexicon::hash_sentiment_jockers_rinker,
  x = lexicon::hash_sentiment_emojis
)

x %>%
  mutate(Tweet = replace_emoji_identifier(Tweet)) %$%
  sentiment(Tweet, polarity_dt = combined_emoji)


## End(Not run)
