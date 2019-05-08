library(tidyverse)
data("iris")
head(iris)
iris %>%
  mutate(z = scale(Sepal.Length, center=TRUE, scale=TRUE)) %>%
  head()
