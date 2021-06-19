library(tidyverse)
library(plotly)

data("mtcars")
df <- mtcars

bubbleplot <- plot_ly(df, x = ~wt, y = ~disp,
                      text = ~cyl, size = ~qsec,
                      color = ~cyl, sizes = c(10, 50),
                      marker =
                        list(opacity = 0.7,
                             sizemode = "diameter"))
bubbleplot <- bubbleplot%>%layout
bubbleplot

