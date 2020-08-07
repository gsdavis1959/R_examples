library(plotly)

library(ggplot2)
set.seed(1234)

dfGamma = data.frame(nu75 = rgamma(100, 0.75),
                     nu1 = rgamma(100, 1),
                     nu2 = rgamma(100, 2))

dfGamma = stack(dfGamma)
dfGamma

p <- ggplot(dfGamma, aes(x = values)) +
  stat_density(aes(group = ind, color = ind),position="identity",geom="line")

fig <- ggplotly(p)

fig

# example filled

carrots <- data.frame(length = rnorm(100000, 6, 2))
cukes <- data.frame(length = rnorm(50000, 7, 2.5))

#Now, combine your two dataframes into one.  First make a new column in each.
carrots$veg <- 'carrot'
cukes$veg <- 'cuke'

#and combine into your new data frame vegLengths
vegLengths <- rbind(carrots, cukes)

#now make your lovely plot
p <- ggplot(vegLengths, aes(length, fill = veg)) + geom_density(alpha = 0.2)

fig <- ggplotly(p)

fig

# example 3
p <- ggplot(diamonds, aes(x = price)) + 
  geom_density(aes(fill = color), alpha = 0.5) + 
  ggtitle("Kernel Density estimates by group")

fig <- ggplotly(p)

fig

# multiple filled

diamonds1 <- diamonds[which(diamonds$cut == "Fair"),]
density1 <- density(diamonds1$carat)

diamonds2 <- diamonds[which(diamonds$cut == "Ideal"),]
density2 <- density(diamonds2$carat)

fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'lines', name = 'Fair cut', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = 'Ideal cut', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Carat'),
                      yaxis = list(title = 'Density'))

fig

# another filled
diamonds1 <- diamonds[which(diamonds$cut == "Fair"),]
density1 <- density(diamonds1$carat)

diamonds2 <- diamonds[which(diamonds$cut == "Ideal"),]
density2 <- density(diamonds2$carat)

fig <- plot_ly(x = ~density1$x, y = ~density1$y, type = 'scatter', mode = 'none', name = 'Fair cut', fill = 'tozeroy',
               fillcolor = 'rgba(168, 216, 234, 0.5)')
fig <- fig %>% add_trace(x = ~density2$x, y = ~density2$y, name = 'Ideal cut', fill = 'tozeroy',
                         fillcolor = 'rgba(255, 212, 96, 0.5)')
fig <- fig %>% layout(xaxis = list(title = 'Carat'),
                      yaxis = list(title = 'Density'))

fig
