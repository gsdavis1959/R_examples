library(tidyverse)
library(dplyr)

df_simple <- data.frame(
  site = c("A","A","B","B"),
  date = as.Date(Sys.Date():(Sys.Date()-3), 
                 origin = "1970-01-01"),
  Phosphorus = c(1:4),
  Nitrate = c(4:1),
  stringsAsFactors = FALSE
)
head(df_simple)

df_simple_long <- df_simple %>% 
  pivot_longer(cols = c(-site, -date),
               names_to = "Chemical",
               values_to = "Value")
head(df_simple_long)


df_with_rmks <- data.frame(
  site = c("A","A","B","B"),
  date = as.Date(Sys.Date():(Sys.Date()-3), 
                 origin = "1970-01-01"),
  Phosphorus_value = c(1:4),
  Phosphorus_rmk = c("<","","",""),
  Nitrate_value = c(4:1),
  Nitrate_rmk = c("","","","<"),
  stringsAsFactors = FALSE
)
head(df_with_rmks)

df_long_with_rmks <- df_with_rmks %>% 
  pivot_longer(cols = c(-site, -date), 
               names_to = c("Chemical", ".value"),
               names_pattern = "(.+)_(.+)")
head(df_long_with_rmks)
