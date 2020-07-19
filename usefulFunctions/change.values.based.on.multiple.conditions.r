library(tidyverse)
data("mtcars")
head(mtcars)

df <- mtcars %>%
  mutate(outcome = "")

transform(df, outcome = case_when(
  am == 0 & mpg > 20 & cyl != 6 ~ "zero", 
  hp == 110 | gear == 4 ~ "one",
  TRUE ~ outcome,
)) %>%
  glimpse()
