library(plotly)

pm <- GGally::ggpairs(iris, aes(color = Species))
class(pm)
#> [1] "gg"  "ggmatrix"
ggplotly(pm)
