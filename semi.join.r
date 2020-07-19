library(tidyverse)

locations <- read.csv('https://covid.ourworldindata.org/data/ecdc/locations.csv', stringsAsFactors = FALSE) %>% print()
names(locations)
full <- read.csv('https://covid.ourworldindata.org/data/ecdc/full_data.csv') %>% print()
names(cases)


by_loc <- left_join(locations, full, by = 'location')%>%
  filter(!is.na(date)) %>% print()

by_loc_inner <- inner_join(locations, full, by = 'location')%>%
  select(-date) %>% print()

by_loc_semi <- semi_join(full, locations, by = 'location')%>%print()
