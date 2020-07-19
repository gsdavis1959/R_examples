library(tidyverse)
library(plotly)

mpg

# multiple traces (less performant, but more interactive)
plot_ly(mpg, x = ~year, y = ~cty) %>%
  add_lines(color = ~ordered(manufacturer))


