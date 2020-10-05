library(plotly)
library(data.table)

data <- mtcars[,c(1,3:7)]
corrdata <- cor(data)


#do this before the transformation!
corrdata[upper.tri(corrdata, diag = TRUE)] <- NA
corrdata <- corrdata[-1, -ncol(corrdata)]

#Store our variable names for later use
x_labels <- colnames(corrdata)
y_labels <- rownames(corrdata)

#Change the variable names to numeric for the grid
colnames(corrdata) <- 1:ncol(corrdata)
rownames(corrdata) <- nrow(corrdata):1

#Melt the data into the desired format
plotdata <- melt(corrdata)

#Adding the size variable & scaling it
plotdata$size <- (abs(plotdata$value))
scaling <- 500 / ncol(corrdata) / 2
plotdata$size <- plotdata$size * scaling

#Setting x and y ranges for the chart
xrange <- c(0.5, length(x_labels)+0.5)
yrange <- c(0.5, length(y_labels)+0.5)

#Setting the gridlines
x_grid <- seq(1.5, length(x_labels)-0.5, 1)
y_grid <- seq(1.5, length(y_labels)-0.5, 1)

#Axes definitions
xAx1 <- list(showgrid = FALSE,
             showline = FALSE,
             zeroline =  FALSE,
             tickvals = colnames(corrdata),
             ticktext = x_labels,
             title = "",
             range = xrange,
             rangemode = "tozero")

xAx2 <- list(showgrid = TRUE,
             showline = FALSE,
             zeroline =  FALSE,
             overlaying = "x",
             showticklabels = FALSE,
             range = xrange,
             tickvals = x_grid)

yAx1 <- list(autoaxis = FALSE,
             showgrid = FALSE,
             showline = FALSE,
             zeroline =  FALSE,
             tickvals = rownames(corrdata),
             ticktext = y_labels,
             title = FALSE,
             rangemode = "tozero",
             range = yrange)

yAx2 <- list(showgrid = TRUE,
             showline = FALSE,
             zeroline =  FALSE,
             overlaying = "y",
             showticklabels = FALSE,
             range = yrange,
             tickvals = y_grid)


fig <- plot_ly(data = plotdata, width = 500, height = 500)
fig <- fig %>% add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                         color = ~value,
                         marker = list(size = ~size, opacity = 1),
                         symbol = I("square"),
                         text = ~value,
                         hovertemplate = "%{text:.2f} <extra></extra>",
                         xaxis = "x1",
                         yaxis = "y1")

fig <- fig %>% add_trace(x = ~Var2, y = ~Var1, type = "scatter", mode = "markers",
                         opacity = 0,
                         showlegend = FALSE,
                         xaxis = "x2",
                         yaxis = "y2",
                         hoverinfo = "none")

fig <- fig %>% layout(xaxis = xAx1,
                      yaxis = yAx1, 
                      xaxis2 = xAx2,
                      yaxis2 = yAx2,
                      plot_bgcolor = "rgba(0,0,0,0)",
                      paper_bgcolor = "rgba(0, 0, 0, 0.03)")

fig <- fig %>% colorbar(title = "", limits = c(-1,1), x = 1.1, y = 0.75)
fig
