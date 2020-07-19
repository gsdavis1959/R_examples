library(tidyverse)

ggplot(stock_summary, aes(year_month, stock_average, group = symbol, colour = symbol)) +
  geom_line() +
  labs(x = "Year/Month", y = "Average Close", 
       title = "Citi Portfolio") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5))
