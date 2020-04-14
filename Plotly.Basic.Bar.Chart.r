library(tidyverse)
library(plotly)
str(mpg)
# multiple traces (less performant, but more interactive)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

# stacked bar chart

fig <- plot_ly(mpg, x = ~manufacturer, y = ~cty, type = 'bar', name = 'City')
fig <- fig %>% add_trace(y = ~hwy, name = 'Highway')
fig <- fig %>% layout(yaxis = list(title = 'Mile per Gallon'), barmode = 'stack')

fig

# flip

mpg$manufacturer <- factor(mpg$manufacturer)

fig2 <- plot_ly(mpg, x = ~cty, y = ~manufacturer, type = 'bar', name = 'City')
fig2 <- fig2 %>% add_trace(x = ~hwy, name = 'Highway')
fig2 <- fig2 %>% layout(xaxis = list(title = 'Mile per Gallon'), barmode = 'stack')

fig2


f1 <- list(
  family = "Arial, sans-serif",
  size = 18,
  bold = TRUE,
  color = "black"
)
f2 <- list(
  family = "Old Standard TT, serif",
  size = 14,
  color = "black"
)
a <- list(
  title = "Manufacturer",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f1
)

b <- list(
  title = "Mpg",
  titlefont = f1,
  showticklabels = TRUE,
  tickfont = f1
)

mpg$manufacturer <- factor(mpg$manufacturer, levels = unique(mpg$manufacturer)[order(mpg$cty, decreasing = TRUE)])
mpg <- mpg %>% mutate(total = cty + hwy)
fig3 <- plot_ly(mpg, x = ~cty, y = ~manufacturer, type = 'bar', name = 'City', orientation = 'h')
# fig3 <- fig3 %>% add_trace(x = ~hwy, name = 'Highway')
fig3 <- fig3 %>% layout(xaxis = list(title = 'Mile per Gallon'), barmode = 'stack')
fig3 <- fig3 %>% layout(xaxis = b, yaxis = a, showlegend = TRUE)

fig3

str(mpg)

# Prepare data: group mean city mileage by manufacturer.
cty_mpg <- aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)  # aggregate
colnames(cty_mpg) <- c("make", "mileage")  # change column names
cty_mpg <- cty_mpg[order(cty_mpg$mileage), ]  # sort
cty_mpg$make <- factor(cty_mpg$make, levels = cty_mpg$make)  # to retain the order in plot.
head(cty_mpg, 4)

library(ggplot2)
theme_set(theme_bw())

# Draw plot
p <- ggplot(cty_mpg, aes(x=make, y=mileage)) + 
  geom_bar(stat="identity", width=.5, fill="darkblue") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
       coord_flip() +
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

ggplotly(p)


# Stacked
f <- ggplot(mpg, aes(fill=hwy, y=cty, x=manufacturer)) + 
  geom_bar(position="stack", stat="identity") +
  coord_flip()

ggplotly(f)
