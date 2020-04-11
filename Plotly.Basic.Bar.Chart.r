library(tidyverse)
library(plotly)
mpg
# multiple traces (less performant, but more interactive)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

# stacked bar chart

fig <- plot_ly(mpg, x = ~trans, y = ~cty, type = 'bar', name = 'City')
fig <- fig %>% add_trace(y = ~hwy, name = 'Highway')
fig <- fig %>% layout(yaxis = list(title = 'Mile per Gallon'), barmode = 'stack')

fig
