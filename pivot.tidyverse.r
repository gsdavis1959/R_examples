devtools::install_github("tidyverse/tidyr")
# library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)

setwd('~/Data/Datasets/Politics')
pew <- read_csv("pew.csv", col_types = list())
pew

pew %>% 
  pivot_long(cols = -religion, names_to = "income", values_to = "count")

setwd('~/Data/Datasets/Songs')
billboard <- read_csv("billboard.csv", col_types = list(time = col_skip()))
billboard

billboard %>% 
  pivot_long(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    na.rm = TRUE
  )

spec <- billboard %>% 
  pivot_long_spec(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank"
  )
spec
