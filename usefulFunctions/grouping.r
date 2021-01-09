library("tidyverse")
df <- read_csv('C:/Users/gsdav/Documents/Data/Datasets/top10s.csv') %>%
  rename(genre = `top genre`) %>% print()

# group by genre
df %>%
  group_by(genre)

# without groups
df %>%
  summarise(summary = mean(bpm))

# with groups
df %>%
  group_by(genre) %>%
  summarise(mean_bpm = mean(bpm))

# mutate without grouping
df %>%
  mutate(mean_bpm = (bpm - mean(bpm))^2) %>%
  select(genre, mean_bpm)

# mutate with grouping
df %>%
  group_by(genre) %>%
  mutate(mean_bpm = (bpm - mean(bpm))^2) %>%
  select(genre, mean_bpm)

# to ungroup

# multiple fields
df %>%
  group_by(genre, year) %>%
  summarise(rec_count = n()) %>%
  arrange(desc(year), desc(rec_count))

# multiple fields
df %>%
  group_by(genre, year) %>%
  summarise(genre_year_count = n()) %>%
  arrange(desc(year), desc(genre_year_count)) %>%
  mutate(genre_count = n()) %>%
  ungroup() %>%
  mutate(total_count = n())                                  

# Using a group_by after the other replaces the previous, 
# but we can set the parameter add to true if we need to perform something like that.

df %>%
  group_by(genre) %>%
  mutate(mean_bpm_genre = mean(bpm)) %>%
  group_by(year, .add = TRUE) %>%
  mutate(mean_bpm_genre_year = mean(bpm)) %>%
  select(genre, year, mean_bpm_genre, mean_bpm_genre_year)

# create a function to group

my_func <- function(df, group){
  df %>%
    group_by(!!group) %>%
    summarise(my_count = n()) %>%
    arrange(desc(my_count))
}
my_group = quo(year)
my_func(df, my_group)


# another function
my_func <- function(df, group){
  df %>%
    group_by(!!group) %>%
    summarise(my_count = n()) %>%
    arrange(desc(my_count))
}
my_func(df, 'genre')


# group by all
new_df <- select(df, genre, year)
new_df %>%
  group_by_all() %>%
  summarise(my_cnt = n()) %>%
  arrange(desc(my_cnt))
new_df %>%
  group_by(across()) %>%
  summarise(my_cnt = n()) %>%
  arrange(desc(my_cnt))

# group_by_if
new_df <- df %>%
  mutate(artist = as.factor(artist),
         genre = as.factor(genre))
new_df %>%
  group_by_if(is.factor) %>%
  summarise(my_cnt = n()) %>%
  arrange(desc(my_cnt))
new_df %>%
  group_by(across(where(is.factor))) %>%
  summarise(my_cnt = n()) %>%
  arrange(desc(my_cnt))

# any record contains the word ‘dance’ on them
new_df %>%
  group_by_if(function(x) any(grepl('dance', x, fixed=TRUE))) %>%
  summarise(my_cnt = n())


# split
df_list <- df %>%
  group_by(year) %>%
  group_split()
df_list[[10]]

# nest 
df_nest <- df %>%
  group_nest(genre, year)
df_nest
