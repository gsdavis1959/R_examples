library(plotly)

Primates <- c('Potar monkey', 'Gorilla', 'Human', 'Rhesus monkey', 'Chimp')
Bodywt <- c(10.0, 207.0, 62.0, 6.8, 52.2)
Brainwt <- c(115, 406, 1320, 179, 440)
data <- data.frame(Primates, Bodywt, Brainwt)

fig <- plot_ly(data, x = ~Bodywt, y = ~Brainwt, type = 'scatter',
               mode = 'text', text = ~Primates, textposition = 'middle right',
               textfont = list(color = '#000000', size = 16))
fig <- fig %>% layout(title = 'Primates Brain and Body Weight',
                      xaxis = list(title = 'Body Weight (kg)',
                                   zeroline = TRUE,
                                   range = c(0, 250)),
                      yaxis = list(title = 'Brain Weight (g)',
                                   range = c(0,1400)))

fig

library(plotly)

data <- mtcars[which(mtcars$am == 1 & mtcars$gear == 4),]

t <- list(
  family = "tahoma",
  size = 14,
  color = toRGB("grey50"))

fig <- plot_ly(data, x = ~wt, y = ~mpg, text = paste('<b>', rownames(data), '</b>'))
fig <- fig %>% add_markers()
fig <- fig %>% add_text(textfont = t, textposition = "top right")
fig <- fig %>% layout(xaxis = list(range = c(1.6, 3.2)),
                      showlegend = FALSE)

fig

df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv", stringsAsFactors = FALSE)
head(df)
df <- df[which(df$year==2007 & df$continent=='Europe' & df$pop > 2.e6),]

fig <- plot_ly(df, type='bar', x = ~country, y = ~pop, text = ~lifeExp, name="",
               hovertemplate = paste('%{x}', '<br>lifeExp: %{text:.2s}<br>'),
               texttemplate = '%{y:.2s}', textposition = 'outside')

fig <- fig %>% layout(uniformtext=list(minsize=8, mode='hide'))
fig

df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/gapminderDataFiveYear.csv", stringsAsFactors = FALSE)
df <- df[which(df$year==2007 & df$continent=='Asia'),]

fig <- plot_ly(df, type='pie', labels = ~country, values = ~pop, textposition = 'inside')
fig <- fig %>% layout(uniformtext=list(minsize=12, mode='hide'))
fig

fig <- plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width, type = 'scatter', mode = 'markers',
               text = ~paste('Species: ', Species))

fig

fig <- plot_ly(iris, x = ~Petal.Length, y = ~Petal.Width, type = 'scatter', mode = 'markers',
               hoverinfo = 'text',
               text = ~paste('</br> Species: ', Species,
                             '</br> Petal Length: ', Petal.Length,
                             '</br> Petal Width: ', Petal.Width))

fig


m <- mtcars[which.max(mtcars$mpg), ]

a <- list(
  x = m$wt,
  y = m$mpg,
  text = rownames(m),
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

fig <- plot_ly(mtcars, x = ~wt, y = ~mpg)
fig <- fig %>% add_markers()
fig <- fig %>% layout(annotations = a)

fig

data <- mtcars[which(mtcars$am == 1 & mtcars$gear == 4),]
head(data)

fig <- plot_ly(data, x = ~wt, y = ~mpg, type = 'scatter', mode = 'markers',
               marker = list(size = 10))
fig <- fig %>% add_annotations(x = data$wt,
                               y = data$mpg,
                               text = rownames(data),
                               xref = "x",
                               yref = "y",
                               showarrow = TRUE,
                               arrowhead = 4,
                               arrowsize = .5,
                               ax = 20,
                               ay = -40)

fig

# subplots

m <- economics[which.max(economics$unemploy), ]
n <- economics[which.max(economics$uempmed), ]

# annotations
a <- list(
  x = m$date,
  y = m$unemploy,
  text = "annotation a",
  xref = "x",
  yref = "y",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

b <- list(
  x = n$date,
  y = n$uempmed,
  text = "annotation b",
  xref = "x2",
  yref = "y2",
  showarrow = TRUE,
  arrowhead = 7,
  ax = 20,
  ay = -40
)

# figure labels
f <- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f ")
x <- list(
  title = "x Axis",
  titlefont = f)
y <- list(
  title = "y Axis",
  titlefont = f)

fig1 <- plot_ly(economics, x = ~date, y = ~unemploy)
fig1 <- fig1 %>% add_lines(name = ~"unemploy")
fig1 <- fig1 %>% layout(annotations = a, xaxis = x, yaxis = y)
fig2 <- plot_ly(economics, x = ~date, y = ~uempmed)
fig2 <- fig2 %>% add_lines(name = ~"uempmed")
fig2 <- fig2 %>% layout(annotations = b, xaxis = x, yaxis = y)
fig <- subplot(fig1, fig2, titleX = TRUE, titleY = TRUE)
fig2 <- fig2 %>% layout(showlegend = FALSE)

fig

# styling
data <- mtcars[which(mtcars$am == 1 & mtcars$gear == 4),]

fig <- plot_ly(data, x = ~wt, y = ~mpg, type = 'scatter', mode = 'markers',
               marker = list(size = 10))
fig <- fig %>% add_annotations(x = data$wt,
                               y = data$mpg,
                               text = rownames(data),
                               xref = "x",
                               yref = "y",
                               showarrow = TRUE,
                               arrowhead = 4,
                               arrowsize = .5,
                               ax = 20,
                               ay = -40,
                               # Styling annotations' text:
                               font = list(color = '#264E86',
                                           family = 'sans serif',
                                           size = 14))

fig

# test anchors
fig <- plot_ly()
fig <- fig %>% add_markers(
  x = 1,
  y = 1,
  showlegend = F
)
fig <- fig %>% add_markers(
  x = 1,
  y = 2,
  showlegend = F
)
fig <- fig %>% add_markers(
  x = 1,
  y = 3,
  showlegend = F
)
fig <- fig %>% add_annotations(
  x=1,
  y=1,
  xref = "x",
  yref = "y",
  text = "Right Anchor",
  xanchor = 'right',
  showarrow = F
)
fig <- fig %>% add_annotations(
  x=1,
  y=2,
  xref = "x",
  yref = "y",
  text = "Center Anchor",
  xanchor = 'center',
  showarrow = F
)
fig <- fig %>% add_annotations(
  x=1,
  y=3,
  xref = "x",
  yref = "y",
  text = "Left Anchor",
  xanchor = 'left',
  showarrow = F
)

fig

# text template
fig <- plot_ly(
  type='pie',
  values=c(40000000, 20000000, 30000000, 10000000),
  labels=c("Wages", "Operating expenses", "Cost of sales", "Insurance"),
  texttemplate="%{label}: %{value:$,s} <br>(%{percent})",
  textposition="inside")
fig

# custom text template
fig <- plot_ly(
  type='scatterternary',
  a = c(3, 2, 5),
  b = c(2, 5, 2),
  c = c(5, 2, 2),
  mode = "markers+text",
  text = c("A", "B", "C"),
  texttemplate = "%{text}<br>(%{a:.2f}, %{b:.2f}, %{c:.2f})",
  textposition = "bottom center",
  textfont = list(family= "Times", size= c(18, 21, 20), color= c("IndianRed", "MediumPurple", "DarkOrange"))
)
fig

fig <- plot_ly()

fig <- fig %>% add_trace(
  type='funnel',
  name = 'Montreal',
  orientation = "h",
  y = c("2018-01-01", "2018-07-01", "2019-01-01", "2020-01-01"),
  x = c(100, 60, 40, 20),
  textposition = "inside",
  texttemplate = "%{y| %a. %_d %b %Y}")

fig <- fig %>% add_trace(
  type='funnel',
  name = 'Vancouver',
  orientation = "h",
  y = c("2018-01-01", "2018-07-01", "2019-01-01", "2020-01-01"),
  x = c(90, 70, 50, 10),
  textposition = "inside",
  textinfo = "label")

fig <- fig %>% layout(yaxis = list(type= 'date'))
fig

fig <- plot_ly()
fig <- fig %>% add_markers(
  x = 0.5,
  y = 1,
  showlegend = F
)
fig <- fig %>% add_annotations(
  x= 0.5,
  y= 1,
  xref = "paper",
  yref = "paper",
  text = "<b>paper reference = [0.5, 1]</b>", # makes font bold
  showarrow = F
)
fig <- fig %>% add_annotations(
  x= 0.5,
  y= 1,
  xref = "x",
  yref = "y",
  text = "x + y reference = [0.5, 1]",
  showarrow = T,
  ax = 20,
  ay = -40
)
fig <- fig %>% layout(
  xaxis = list(zeroline = F),
  yaxis = list(zeroline = F)
)

fig
