library(tidyverse)

data("diamonds")

average_price <- diamonds %>% 
  group_by(cut) %>%
  summarise(average_price = mean(price)) %>%
  ungroup() 
average_price

average_price %>% 
  ggplot() +
  geom_col(aes(x = cut, y = average_price))

library(scales)
show_col(c("#9C89B8", "#F0A6CA", "#EFC3E6", "#F0E6EF"))
