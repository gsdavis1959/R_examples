library(tidyverse)

df <- read.csv('~/Data/Datasets/test.csv', stringsAsFactors = FALSE)
head(df)

df <- df %>% 
  rename("A" = ï..A) %>% 
  print()

df %>%
  mutate(A_new = 
           B %>% 
           is.na %>%
           ifelse(A, B) )
