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



### EXAMPLE 2 ####

head(iris)
print(unique(iris$Species)[1])

p <- iris %>%
  plot_ly(
    type = 'scatter', 
    x = ~Sepal.Length, 
    y = ~Petal.Length,
    text = ~Species,
    hoverinfo = 'text',
    mode = 'markers', 
    transforms = list(
      list(
        type = 'filter',
        target = ~Species,
        operation = '=',
        value = unique(iris$Species)[1]
      )
    )) %>% layout(
      updatemenus = list(
        list(
          type = 'dropdown',
          active = 0,
          buttons = list(
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[1]),
                 label = unique(iris$Species)[1]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[2]),
                 label = unique(iris$Species)[2]),
            list(method = "restyle",
                 args = list("transforms[0].value", unique(iris$Species)[3]),
                 label = unique(iris$Species)[3])
          )
        )
      )
    )
p


### EXAMPLE 3 ###

fig <- plot_ly(
  type = 'scatter',
  x = mtcars$hp,
  y = mtcars$qsec,
  text = rownames(mtcars),
  hoverinfo = 'text',
  mode = 'markers',
  transforms = list(
    list(
      type = 'filter',
      target = 'y',
      operation = '>',
      value = mean(mtcars$qsec)
    )
  )
)

fig


### EXAMPLE 4 ###

x0 <- rnorm(400, mean=2, sd=0.4)
y0 <- rnorm(400, mean=2, sd=0.4)
x1 <- rnorm(400, mean=3, sd=0.6)
y1 <- rnorm(400, mean=6, sd=0.4)
x2 <- rnorm(400, mean=4, sd=0.2)
y2 <- rnorm(400, mean=4, sd=0.4)

# shapes components
cluster0 = list(
  type = 'circle',
  xref ='x', yref='y',
  x0=min(x0), y0=min(y0),
  x1=max(x0), y1=max(y0),
  opacity=0.25,
  line = list(color="#835AF1"),
  fillcolor="#835AF1")

cluster1 = list(
  type = 'circle',
  xref ='x', yref='y',
  x0=min(x1), y0=min(y1),
  x1=max(x1), y1=max(y1),
  opacity=0.25,
  line = list(color="#7FA6EE"),
  fillcolor="#7FA6EE")

cluster2 = list(
  type = 'circle',
  xref ='x', yref='y',
  x0=min(x2), y0=min(y2),
  x1=max(x2), y1=max(y2),
  opacity=0.25,
  line = list(color="#B8F7D4"),
  fillcolor="#B8F7D4")

# updatemenus component
updatemenus <- list(
  list(
    active = -1,
    type = 'buttons',
    buttons = list(
      
      list(
        label = "None",
        method = "relayout",
        args = list(list(shapes = c()))),
      
      list(
        label = "Cluster 0",
        method = "relayout",
        args = list(list(shapes = list(cluster0, c(), c())))),
      
      list(
        label = "Cluster 1",
        method = "relayout",
        args = list(list(shapes = list(c(), cluster1, c())))),
      
      list(
        label = "Cluster 2",
        method = "relayout",
        args = list(list(shapes = list(c(), c(), cluster2)))),
      
      list(
        label = "All",
        method = "relayout",
        args = list(list(shapes = list(cluster0,cluster1,cluster2))))
    )
  )
)

fig <- plot_ly(type = 'scatter', mode='markers') 
fig <- fig %>% add_trace(x=x0, y=y0, mode='markers', marker=list(color='#835AF1')) 
fig <- fig %>% add_trace(x=x1, y=y1, mode='markers', marker=list(color='#7FA6EE')) 
fig <- fig %>% add_trace(x=x2, y=y2, mode='markers', marker=list(color='#B8F7D4')) 
fig <- fig %>% layout(title = "Highlight Clusters", showlegend = FALSE,
                      updatemenus = updatemenus)

fig


### EXAMPLE 5 ###

plot_ly(mtcars, x = rownames(mtcars), y = ~mpg, name='mpg', type='scatter', mode='markers') %>%
  add_trace(y = ~hp, name = 'power', type='scatter', mode='markers') %>%
  add_trace(y = ~qsec, name = 'qsec', type='scatter', mode='markers') %>%
  layout(
    updatemenus = list(
      list(
        type = "buttons",
        x = -0.1,
        y = 0.6,
        label = 'Category',
        buttons = list(
          list(method = "restyle",
               args = list('visible', c(TRUE, TRUE, TRUE)),
               label = "View all"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, FALSE)),
               label = "Hide all")
        )
      ), 
      list(
        type = "buttons",
        x = -0.1,
        y = 0.7,
        label = 'Category',
        buttons = list(
          list(method = "restyle",
               args = list('visible', c(TRUE, FALSE, FALSE)),
               label = "mpg"),
          list(method = "restyle",
               args = list('visible', c(FALSE, TRUE, FALSE)),
               label = "hp"),
          list(method = "restyle",
               args = list('visible', c(FALSE, FALSE, TRUE)),
               label = "qsec")
        )
      )
    )
  )
