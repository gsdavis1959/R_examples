library(tidyverse)

library("hflights") 
flights <-tbl_df(hflights) 
flights

f2 <- flights %>% mutate(tail = TailNum, 
                         new_var = ifelse(UniqueCarrier == 'AA', TailNum, NA)) %>%
  select(UniqueCarrier, AirTime, TailNum, tail, new_var)
