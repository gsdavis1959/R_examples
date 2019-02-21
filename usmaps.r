library(usmap)
library(ggplot2)

plot_usmap(include = c("CA", "ID", "NV", "OR", "WA")) +
  labs(title = "Western US States", subtitle = "These are the states in the Pacific Timezone.")


plot_usmap(data = statepop, values = "pop_2015") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = statepop, values = "pop_2015", lines = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + theme(legend.position = "right")
