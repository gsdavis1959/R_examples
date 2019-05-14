setwd('~/Data/Datasets/Psychology')
df <- read.csv('survey.csv', stringsAsFactors = FALSE)
df
df <- df %>%
  mutate(id = row.names.data.frame(df))
df
library(tidyverse)
library(reshape2)
df.wide <- dcast(df, Person ~ Question, value.var = "Answer")
df.wide
