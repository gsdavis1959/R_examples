library(tidyverse)
library(plotly)

# multiple traces (less performant, but more interactive)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")
