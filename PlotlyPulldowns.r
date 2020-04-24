library(plotly)
library(MASS)

covmat <- matrix(c(0.8, 0.4, 0.3, 0.8), nrow = 2, byrow = T)
df <- mvrnorm(n = 10000, c(0,0), Sigma = covmat)
df <- as.data.frame(df)

colnames(df) <- c("x", "y")
fig <- plot_ly(df, x = ~x, y = ~y, alpha = 0.3)
fig <- fig %>% add_markers(marker = list(line = list(color = "black", width = 1)))
fig <- fig %>% layout(
  title = "Drop down menus - Plot type",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"),
  updatemenus = list(
    list(
      y = 0.8,
      buttons = list(
        
        list(method = "restyle",
             args = list("type", "scatter"),
             label = "Scatter"),
        
        list(method = "restyle",
             args = list("type", "histogram2d"),
             label = "2D Histogram")))
  ))

fig

x <- seq(-2 * pi, 2 * pi, length.out = 1000)
df <- data.frame(x, y1 = sin(x), y2 = cos(x))
df

fig <- plot_ly(df, x = ~x)
fig <- fig %>% add_lines(y = ~y1, name = "A")
fig <- fig %>% add_lines(y = ~y2, name = "B", visible = F)
fig <- fig %>% layout(
  title = "Drop down menus - Styling",
  xaxis = list(domain = c(0.1, 1)),
  yaxis = list(title = "y"),
  updatemenus = list(
    list(
      y = 0.8,
      buttons = list(
        
        list(method = "restyle",
             args = list("line.color", "blue"),
             label = "Blue"),
        
        list(method = "restyle",
             args = list("line.color", "red"),
             label = "Red"))),
    
    list(
      y = 0.7,
      buttons = list(
        list(method = "restyle",
             args = list("visible", list(TRUE, FALSE)),
             label = "Sin"),
        
        list(method = "restyle",
             args = list("visible", list(FALSE, TRUE)),
             label = "Cos")))
  )
)

fig
