library(skimr)
library(tidyverse)

data(iris)
skim(iris)

group_by(iris, Petal.Width) %>% skim()
