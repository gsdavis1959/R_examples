library(plotly)

df1 = read.csv('https://raw.githubusercontent.com/plotly/datasets/718417069ead87650b90472464c7565dc8c2cb1c/sunburst-coffee-flavors-complete.csv')
df2 = read.csv('https://raw.githubusercontent.com/plotly/datasets/718417069ead87650b90472464c7565dc8c2cb1c/coffee-flavors.csv')
head(df1)
head(df2)

fig <- plot_ly(
  type='treemap',
  ids=df1$ids,
  labels=df1$labels,
  parents=df1$parents,
  domain=list(column=0))

fig <- fig %>% add_trace(
  type='treemap',
  ids=df2$ids,
  labels=df2$labels,
  parents=df2$parents,
  maxdepth=1,
  domain=list(column=1))
fig <- fig %>% layout(grid=list(columns=2, rows=1))
fig


labels = c("A1", "A2", "A3", "A4", "A5", "B1", "B2")
parents = c("", "A1", "A2", "A3", "A4", "", "B1")
values = c("11", "12", "13", "14", "15", "20", "30")

fig2 <- plot_ly(
  type="treemap",
  labels=labels,
  parents=parents,
  values=values,
  marker=list(colorscale='Reds'))

fig2
