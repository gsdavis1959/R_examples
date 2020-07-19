# Load the plotly package
library(plotly)

# Data: mtcars:
data <- as.matrix(mtcars)
head(data)
# basic heatmap
p <- plot_ly(x=colnames(data), y=rownames(data), z = data, type = "heatmap") %>%
  layout(margin = list(l=120))
p

# save the widget
library(htmlwidgets)


# Heatmap
p1 <- plot_ly(x=colnames(data), y=rownames(data), 
             z = data, 
             type = "heatmap", 
             colorscale= "Earth",
             showscale = F) %>%
  layout(margin = list(l=120))
p1


saveWidget(p1, file="plotlyHeatmap2.html")
