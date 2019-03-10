# load libraries
library(tidyverse)
library(dplyr)
library(magrittr)

# get data
library(fivethirtyeight)
# data(package = "fivethirtyeight")
hiphop_cand_lyrics <- fivethirtyeight::hiphop_cand_lyrics %>% tbl_df
hiphop_cand_lyrics

# select
hiphop_cand_lyrics %>% 
  dplyr::select(candidate) %>% 
  glimpse()

hiphop_cand_lyrics %>% 
  dplyr::select(
    candidate,
    date = album_release_date,
    song,
    artist,
    sentiment,
    theme,
    line) %>% glimpse()
hiphop_cand_lyrics %>% 
  dplyr::select(
    -(url),
    date = album_release_date) %>% 
  glimpse()

(hiphop <- hiphop_cand_lyrics %>% 
    dplyr::select(
      candidate,
      date = album_release_date,
      song,
      artist,
      sentiment,
      theme,
      line))
# match a pattern (variables with t)
hiphop %>% 
  dplyr::select(matches("t")) %>% 
  glimpse()

# filter rows
hiphop %>% filter(candidate == "Jeb Bush") %>% glimpse()
# more than one criteria
hiphop %>% 
  filter(candidate %in% c("Jeb Bush", 
                          "Chris Christie")) %>% 
  glimpse()
hiphop %>% 
  filter(date > 2005 & date <= 2014) %>% # all observations after 2005 and before or on 2014
  glimpse() 
hiphop %>% filter_all(any_vars(. > 2005)) %>% glimpse()

# mutate - create a new variable based on existing variables
hiphop %>% 
  mutate(new_2010 = 
           if_else(date >= 2010, "new song", "not new")) %>% 
  glimpse()
hiphop %>% 
  mutate(fake_news = 
           if_else(grepl("hood", line), 
                   "hood", "zero hood")) %>% 
  filter(fake_news == "hood") %>% glimpse()

# count
hiphop %>% 
  count(theme)

hiphop %>% 
  mutate(fake_news = 
           if_else(grepl("hood", line), 
                   "hood", "zero hood")) %>% 
  filter(fake_news == "hood") %>% 
  count(sentiment, fake_news) %>% 
  spread(sentiment, n)

hiphop %>% 
  mutate(lewd_lang = 
           if_else(grepl("happy", line), 
                   "happy", "zero happy")) %>% 
  count(sentiment, lewd_lang) %>% 
  mutate(prop = prop.table(n)) %>% 
  spread(sentiment, n)
