# to install book code
# if (!require(remotes)) install.packages("remotes")
# remotes::install_github("cpsievert/plotly_book")


library(tidyverse)
library(plotly)
m <- highlight_key(mpg)
p <- ggplot(m, aes(displ, hwy)) + geom_point()
gg <- highlight(ggplotly(p), "plotly_selected")
crosstalk::bscols(gg, DT::datatable(m))

library(ggforce)
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() +
  geom_mark_hull(aes(filter = model == "corvette", label = model)) +
  labs(
    title = "Fuel economy data from 1999 and 2008 for 38 popular models of car",
    caption = "Source: https://fueleconomy.gov/",
    x = "Engine Displacement", 
    y = "Miles Per Gallon"
  )

plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")

# create three visualizations of the diamonds dataset
plot_ly(diamonds, x = ~cut)
plot_ly(diamonds, x = ~cut, y = ~clarity)
plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Blues")

# doesn't produce black bars
plot_ly(diamonds, x = ~cut, color = "black")
# produces red bars with black outline
plot_ly(diamonds, x = ~cut, color = I("red"), stroke = I("black"), span = I(2))

diamonds %>%
  dplyr::count(cut) %>%
  plot_ly() %>% 
  add_bars(x = ~cut, y = ~n)

diamonds %>%
  plot_ly(x = ~cut) %>% 
  add_histogram() %>%
  group_by(cut) %>%
  summarise(n = n()) %>%
  add_text(
    text = ~scales::comma(n), y = ~n, 
    textposition = "top middle", 
    cliponaxis = FALSE
  )
# see json of plot
p <- plot_ly(diamonds, x = ~cut, color = ~clarity, colors = "Accent")
plotly_json(p)

# use plotly_build() to get at the plotly.js definition
# behind *any* plotly object
b <- plotly_build(p)

# Confirm there 8 traces
length(b$x$data)
#> [1] 8

# Extract the `name` of each trace. plotly.js uses `name` to 
# populate legend entries and tooltips
purrr::map_chr(b$x$data, "name")
#> [1] "IF" "VVS1" "VVS2" "VS1" "VS2" "SI1" "SI2" "I1" 

# Every trace has a type of histogram
unique(purrr::map_chr(b$x$data, "type"))
#> [1] "histogram"

p <- ggplot(diamonds, aes(x = log(carat), y = log(price))) + 
  geom_hex(bins = 100)
ggplotly(p)

p <- ggplot(diamonds, aes(x = log(price), color = clarity)) + 
  geom_freqpoly()
ggplotly(p)

# facets
p <- ggplot(diamonds, aes(x = log(price), color = clarity)) + 
  geom_freqpoly(stat = "density") + 
  facet_wrap(~cut)
ggplotly(p)
# format as html so it can be saved as a file from the viewer
toWebGL(ggplotly(p))


library(GGally)
m <- lm(log(price) ~ log(carat) + cut, data = diamonds)
gg <- ggcoef(m)
# dynamicTicks means generate new axis ticks on zoom
ggplotly(gg, dynamicTicks = TRUE)

# missing values
library(naniar)
# fake some missing data
diamonds$price_miss <- ifelse(diamonds$depth > 60, diamonds$price, NA)
p <- ggplot(diamonds, aes(x = clarity, y = log(price_miss))) +
  geom_miss_point(alpha = 0.1) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  facet_wrap(~cut)
toWebGL(ggplotly(p))

data(economics, package = "ggplot2")

# sort economics by psavert, just to 
# show difference between paths and lines
p <- economics %>%
  arrange(psavert) %>%
  plot_ly(x = ~date, y = ~psavert)

add_paths(p)
add_lines(p)

library(lubridate)
econ <- economics %>%
  mutate(yr = year(date), mnth = month(date))

# one trace (more performant, but less interactive)
econ %>%
  group_by(yr) %>%
  plot_ly(x = ~mnth, y = ~uempmed) %>%
  add_lines(text = ~yr)

# multiple traces (less performant, but more interactive)
plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
  add_lines(color = ~ordered(yr))

# the split argument guarantees one trace per group level (regardless of the variable type)
# this is useful if you want a consistent visual properties over multiple traces 
plot_ly(econ, x = ~mnth, y = ~uempmed) %>%
   add_lines(split = ~yr, color = I("black"))
# annotations
set.seed(99)
plot_ly() %>%
  add_trace(
    type = "scatter",
    mode = "markers+lines+text",
    x = 4:6, 
    y = 4:6,
    text = replicate(3, praise::praise("You are ${adjective}! 🙌")),
    textposition = "right",
    hoverinfo = "text",
    textfont = list(family = "Roboto Condensed", size = 16)
  ) %>%
  layout(xaxis = list(range = c(3, 8)))
# show attributes
schema()

# subplots - multiple plots in a fram
subplot(
  plot_ly(mpg, x = ~cty, y = ~hwy, name = "default"),
  plot_ly(mpg, x = ~cty, y = ~hwy) %>% 
    add_markers(alpha = 0.2, name = "alpha")
)

# colors
p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.5)
subplot(
  add_markers(p, color = ~cyl, showlegend = FALSE) %>% 
    colorbar(title = "Viridis"),
  add_markers(p, color = ~factor(cyl))
)

col1 <- c("#132B43", "#56B1F7")
col2 <- viridisLite::inferno(10)
col3 <- colorRamp(c("red", "white", "blue"))
subplot(
  add_markers(p, color = ~cyl, colors = col1) %>%
    colorbar(title = "ggplot2 default"),
  add_markers(p, color = ~cyl, colors = col2) %>% 
    colorbar(title = "Inferno"),
  add_markers(p, color = ~cyl, colors = col3) %>% 
    colorbar(title = "colorRamp")
) %>% hide_legend()

col1 <- "Accent"
col2 <- colorRamp(c("red", "blue"))
col3 <- c(`4` = "red", `5` = "black", `6` = "blue", `8` = "green")
subplot(
  add_markers(p, color = ~factor(cyl), colors = col1),
  add_markers(p, color = ~factor(cyl), colors = col2),
  add_markers(p, color = ~factor(cyl), colors = col3)
) %>% hide_legend()
add_markers(p, color = I("black"))

# markers
p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3) 
subplot(
  add_markers(p, symbol = ~cyl, name = "A single trace"),
  add_markers(p, symbol = ~factor(cyl), color = I("black"))
)

subplot(
  add_markers(p, symbol = ~cyl, symbols = c(17, 18, 19)),
  add_markers(
    p, color = I("black"),
    symbol = ~factor(cyl), symbols = c("triangle-up", "diamond", "circle")
  )
)

plot_ly(mpg, x = ~cty, y = ~hwy) %>%
  add_markers(symbol = I(18), alpha = 0.5)
plot_ly(mpg, x = ~cty, y = ~hwy) %>%
  add_markers(symbol = I(18), alpha = 0.5, stroke = I("black"), span = I(1))

# size
p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3) 
subplot(
  add_markers(p, size = ~cyl, name = "default"),
  add_markers(p, size = ~cyl, sizes = c(1, 500), name = "custom")
)
plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3, marker = list(size = 10))

# error bars
library(broom)
library(forcats)

# Fit a full-factorial linear model
m <- lm(Sepal.Length ~ Sepal.Width * Petal.Length * Petal.Width, data = iris)

# (1) get a tidy() data structure of covariate-level info (e.g., point estimate, standard error, etc)
# (2) make sure term column is a factor ordered by the estimate
# (3) plot estimate by term with an error bar for the standard error
tidy(m) %>% 
  mutate(term = fct_reorder(term, estimate)) %>%
  plot_ly(x = ~estimate, y = ~term) %>%
  add_markers(
    error_x = ~list(value = std.error), 
    color = I("black"),
    hoverinfo = "x"
  )
# lines

top5 <- txhousing %>%
  group_by(city) %>%
  summarise(m = mean(sales, na.rm = TRUE)) %>%
  arrange(desc(m)) %>%
  top_n(5)

tx5 <- semi_join(txhousing, top5, by = "city")

plot_ly(tx5, x = ~date, y = ~median) %>%
  add_lines(linetype = ~city)

ltys <- c(
  Austin = "dashdot",
  `Collin County` = "longdash",
  Dallas = "dash",
  Houston = "solid",
  `San Antonio` = "dot"
)

plot_ly(tx5, x = ~date, y = ~median) %>%
  add_lines(linetype = ~city, linetypes = ltys)

# barbell
mpg %>%
  group_by(model) %>%
  summarise(c = mean(cty), h = mean(hwy)) %>%
  mutate(model = forcats::fct_reorder(model, c)) %>%
  plot_ly() %>%
  add_segments(
    x = ~c, y = ~model,
    xend = ~h, yend = ~model, 
    color = I("gray"), showlegend = FALSE
  ) %>%
  add_markers(
    x = ~c, y = ~model, 
    color = I("blue"), 
    name = "mpg city"
  ) %>%
  add_markers(
    x = ~h, y = ~model, 
    color = I("red"),
    name  = "mpg highway"
  ) %>%
  layout(xaxis = list(title = "Miles per gallon"))

# candlestick
library(quantmod)
msft <- getSymbols("MSFT", auto.assign = F)
dat <- as.data.frame(msft)
dat$date <- index(msft)
dat <- subset(dat, date >= "2016-01-01")

names(dat) <- sub("^MSFT\\.", "", names(dat))

plot_ly(dat, x = ~date, xend = ~date, color = ~Close > Open, 
        colors = c("red", "forestgreen"), hoverinfo = "none") %>%
  add_segments(y = ~Low, yend = ~High, size = I(1)) %>%
  add_segments(y = ~Open, yend = ~Close, size = I(3)) %>%
  layout(showlegend = FALSE, yaxis = list(title = "Price")) %>%
  rangeslider()
# density plot
kerns <- c("gaussian", "epanechnikov", "rectangular", 
           "triangular", "biweight", "cosine", "optcosine")
p <- plot_ly()
for (k in kerns) {
  d <- density(economics$pce, kernel = k, na.rm = TRUE)
  p <- add_lines(p, x = d$x, y = d$y, name = k)
}
p

# map
base <- map_data("world", "us") %>%
  group_by(group) %>%
  plotly_empty(x = ~long, y = ~lat, alpha = 0.2) %>%
  layout(showlegend = FALSE, xaxis = list(scaleanchor = "y"))

base %>%
  add_polygons(hoverinfo = "none", color = I("black")) %>%
  add_markers(text = ~paste(name, "<br />", pop), hoverinfo = "text", 
              color = I("red"), data = maps::us.cities)
add_polygons(base, split = ~subregion, hoveron = "fills")

# ribbons
m <- lm(mpg ~ wt, data = mtcars)
broom::augment(m) %>%
  plot_ly(x = ~wt, showlegend = FALSE) %>%
  add_markers(y = ~mpg, color = I("black")) %>%
  add_ribbons(ymin = ~.fitted - 1.96 * .se.fit, 
              ymax = ~.fitted + 1.96 * .se.fit, color = I("gray80")) %>%
  add_lines(y = ~.fitted, color = I("steelblue"))

Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1IjoiZ3NkYXZpczE5NTkiLCJhIjoiY2p1NjFneHR6MHlvYzRlczdsc21ub3BpOCJ9.ad1YvDnrGkKtn-GUpPdGdA')
plot_mapbox(maps::canada.cities) %>%
  add_markers(
    x = ~long, 
    y = ~lat, 
    size = ~pop, 
    color = ~country.etc,
    colors = "Accent",
    text = ~paste(name, pop),
    hoverinfo = "text"
  )

styles <- schema()$layout$layoutAttributes$mapbox$style$values
styles
layout(
  plot_mapbox(maps::canada.cities), 
  mapbox = list(style = "satellite")
)
style_buttons <- lapply(styles, function(s) {
  list(label = s, method = "relayout", args = list("mapbox.style", s))
})
layout(
  plot_mapbox(maps::canada.cities), 
  mapbox = list(style = "dark"),
  updatemenus = list(
    list(y = 0.8, buttons = style_buttons)
  )
)
map1 <- plot_mapbox() %>% 
  add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
  layout(
    mapbox = list(
      zoom = 0,
      center = list(lat = 65, lon = -75)
    )
  )

map2 <- plot_geo() %>% 
  add_segments(x = -100, xend = -50, y = 50, yend = 75) %>%
  layout(geo = list(projection = list(type = "mercator")))

library(htmltools)
browsable(tagList(map1, map2))

# Choropleths
density <- state.x77[, "Population"] / state.x77[, "Area"]

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  lakecolor = toRGB('white')
)

plot_geo() %>%
  add_trace(
    z = ~density, text = state.name, span = I(0),
    locations = state.abb, locationmode = 'USA-states'
  ) %>%
  layout(geo = g)

library(sf)
library(rnaturalearth)
world <- ne_countries(returnclass = "sf")
class(world)
#> [1] "sf"    "data.frame"
plot_ly(world, color = I("gray90"), stroke = I("black"), span = I(1))

world %>%
  select(name) %>%
  print(n = 4)
canada <- ne_states(country = "Canada", returnclass = "sf")
plot_ly(canada, split = ~name, color = ~provnum_ne)

plot_ly(
  canada, 
  split = ~name, 
  color = I("gray90"), 
  text = ~paste(name, "is \n province number", provnum_ne),
  hoveron = "fills",
  hoverinfo = "text",
  showlegend = FALSE
)

# filter the world sf object down to canada
canada <- filter(world, name == "Canada")
# coerce cities lat/long data to an official sf object
cities <- st_as_sf(
  maps::canada.cities, 
  coords = c("long", "lat"),
  crs = 4326
)

# A PROJ4 projection designed for Canada
# http://spatialreference.org/ref/sr-org/7/
# http://spatialreference.org/ref/sr-org/7/proj4/
moll_proj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# perform the projections
canada <- st_transform(canada, moll_proj)
cities <- st_transform(cities, moll_proj)

# plot with geom_sf()
p <- ggplot() + 
  geom_sf(data = canada) +
  geom_sf(data = cities, aes(size = pop), color = "red", alpha = 0.3)
ggplotly(p)

sum(rapply(world$geometry, nrow))
#> [1] 10586

world_large <- ne_countries(scale = "large", returnclass = "sf")
sum(rapply(world_large$geometry, nrow))
#> [1] 548121
plot_mapbox(world_large, color = NA, stroke = I("black"), span = I(0.5))

library(cartogram)
# devtools::install_github("hrbrmstr/albersusa")
library(albersusa)

us_cont <- cartogram_cont(usa_sf("laea"), "pop_2014")

us <- usa_sf("laea")
us_dor <- cartogram_dorling(us, "pop_2014")

plot_ly(stroke = I("black"), span = I(1)) %>% 
  add_sf(
    data = us, 
    color = I("gray95"),
    hoverinfo = "none"
  ) %>%
  add_sf(
    data = us_dor, 
    color = ~pop_2014,
    split = ~name, 
    text = ~paste(name, scales::number_si(pop_2014)), 
    hoverinfo = "text", 
    hoveron = "fills"
  ) %>%
  layout(showlegend = FALSE)
library(geojsonio)
tiles <- geojson_read("~/Downloads/tiles.topo.json", what = "sp")
tiles_sf <- st_as_sf(tiles)
plot_ly(tiles_sf, split = ~name)

# histograms
p1 <- plot_ly(diamonds, x = ~price) %>% add_histogram(name = "plotly.js")

price_hist <- function(method = "FD") {
  h <- hist(diamonds$price, breaks = method, plot = FALSE)
  plot_ly(x = h$mids, y = h$counts) %>% add_bars(name = method)
}

subplot(
  p1, price_hist(), price_hist("Sturges"),  price_hist("Scott"),
  nrows = 4, shareX = TRUE
)

p1 <- plot_ly(diamonds, x = ~cut) %>% add_histogram()

p2 <- diamonds %>%
  count(cut) %>%
  plot_ly(x = ~cut, y = ~n) %>% 
  add_bars()

subplot(p1, p2) %>% hide_legend()

one_plot <- function(d) {
  plot_ly(d, x = ~price) %>%
    add_annotations(
      ~unique(clarity), x = 0.5, y = 1, 
      xref = "paper", yref = "paper", showarrow = FALSE
    )
}

diamonds %>%
  split(.$clarity) %>%
  lapply(one_plot) %>% 
  subplot(nrows = 2, shareX = TRUE, titleX = FALSE) %>%
  hide_legend()

plot_ly(diamonds, x = ~cut, color = ~clarity) %>%
  add_histogram()

# number of diamonds by cut and clarity (n)
cc <- count(diamonds, cut, clarity)
# number of diamonds by cut (nn)
cc2 <- left_join(cc, count(cc, cut, wt = n, name = 'nn'))
cc2 %>%
  mutate(prop = n / nn) %>%
  plot_ly(x = ~cut, y = ~prop, color = ~clarity) %>%
  add_bars() %>%
  layout(barmode = "stack")

library(ggmosaic)
p <- ggplot(data = cc) +
  geom_mosaic(aes(weight = n, x = product(cut), fill = clarity))
ggplotly(p)

# boxplots
p <- plot_ly(diamonds, y = ~price, color = I("black"), 
             alpha = 0.1, boxpoints = "suspectedoutliers")
p1 <- p %>% add_boxplot(x = "Overall")
p2 <- p %>% add_boxplot(x = ~cut)
subplot(
  p1, p2, shareY = TRUE,
  widths = c(0.2, 0.8), margin = 0
) %>% hide_legend()

plot_ly(diamonds, x = ~price, y = ~interaction(clarity, cut)) %>%
  add_boxplot(color = ~clarity) %>%
  layout(yaxis = list(title = ""))

d <- diamonds %>%
  mutate(cc = interaction(clarity, cut))

# interaction levels sorted by median price
lvls <- d %>%
  group_by(cc) %>%
  summarise(m = median(price)) %>%
  arrange(m) %>%
  pull(cc)

plot_ly(d, x = ~price, y = ~factor(cc, lvls)) %>%
  add_boxplot(color = ~clarity) %>%
  layout(yaxis = list(title = ""))

p <- plot_ly(diamonds, x = ~log(carat), y = ~log(price))
subplot(
  add_histogram2d(p) %>%
    colorbar(title = "default") %>%
    layout(xaxis = list(title = "default")),
  add_histogram2d(p, zsmooth = "best") %>%
    colorbar(title = "zsmooth") %>%
    layout(xaxis = list(title = "zsmooth")),
  add_histogram2d(p, nbinsx = 60, nbinsy = 60) %>%
    colorbar(title = "nbins") %>%
    layout(xaxis = list(title = "nbins")),
  shareY = TRUE, titleX = TRUE
)

kde_count <- function(x, y, ...) {
  kde <- MASS::kde2d(x, y, ...)
  df <- with(kde, setNames(expand.grid(x, y), c("x", "y")))
  # The 'z' returned by kde2d() is a proportion, 
  # but we can scale it to a count
  df$count <- with(kde, c(z) * length(x) * diff(x)[1] * diff(y)[1])
  data.frame(df)
}

kd <- with(diamonds, kde_count(log(carat), log(price), n = 30))
plot_ly(kd, x = ~x, y = ~y, z = ~count) %>% 
  add_heatmap() %>%
  colorbar(title = "Number of diamonds")

# corrplot ----- USE with numeric columns
View(diamonds)
corr <- cor(dplyr::select_if(diamonds, is.numeric))
plot_ly(x = rownames(corr), y = colnames(corr), z = corr, colors = "RdBu") %>%
  add_heatmap() %>%
  colorbar(limits = c(-1, 1))

# 3D
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_markers(color = ~cyl)
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_paths(color = ~displ)
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_lines(color = ~displ)
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  group_by(cyl) %>%
  add_lines(color = ~displ)
# with axes
plot_ly(mpg, x = ~cty, y = ~hwy, z = ~cyl) %>%
  add_lines(color = ~displ) %>%
  layout(
    scene = list(
      xaxis = list(title = "MPG city"),
      yaxis = list(title = "MPG highway"),
      zaxis = list(title = "Number of cylinders")
    )
  )
# surfaces
x <- seq_len(nrow(volcano)) + 100
y <- seq_len(ncol(volcano)) + 500
plot_ly() %>% add_surface(x = ~x, y = ~y, z = ~volcano)

# html files
p <- plot_ly(x = 1:10, y = 1:10) %>% add_markers()
widget_file_size <- function(p) {
  d <- tempdir()
  withr::with_dir(d, htmlwidgets::saveWidget(p, "index.html"))
  f <- file.path(d, "index.html")
  mb <- round(file.info(f)$size / 1e6, 3)
  message("File is: ", mb," MB")
}
widget_file_size(p)
#> File is: 3.244 MB
widget_file_size(partial_bundle(p))
#> File is: 1.045 MB
getwd()

library(htmlwidgets)
p <- plot_ly(x = rnorm(100))
saveWidget(p, "p1.html", selfcontained = F, libdir = "lib")
saveWidget(p, "p2.html", selfcontained = F, libdir = "lib")
zip("p1.zip", c("p1.html", "lib"))
zip("p2.zip", c("p2.html", "lib"))

# set size and format
plot_ly(width = 8 * 96, height = 11 * 96) %>%
  config(toImageButtonOptions = list(format = "svg"))

# arranging plots

p1 <- plot_ly(economics, x = ~date, y = ~unemploy) %>% 
  add_lines(name = "unemploy")
p2 <- plot_ly(economics, x = ~date, y = ~uempmed) %>% 
  add_lines(name = "uempmed")
subplot(p1, p2)

vars <- setdiff(names(economics), "date")
plots <- lapply(vars, function(var) {
  plot_ly(economics, x = ~date, y = as.formula(paste0("~", var))) %>%
    add_lines(name = var)
})
subplot(plots, nrows = length(plots), shareX = TRUE, titleX = FALSE)

# scatterplots
dims <- dplyr::select_if(iris, is.numeric)
dims <- purrr::map2(dims, names(dims), ~list(values = .x, label = .y))
plot_ly(
  type = "splom", dimensions = setNames(dims, NULL), 
  showupperhalf = FALSE, diagonal = list(visible = FALSE)
)

# generalized pairs
pm <- GGally::ggpairs(iris, aes(color = Species))
class(pm)
#> [1] "gg"  "ggmatrix"
ggplotly(pm)

# trellis

library(trelliscopejs)
data(gapminder, package = "gapminder")

qplot(year, lifeExp, data = gapminder) +
  xlim(1948, 2011) + ylim(10, 95) + theme_bw() +
  facet_trelliscope(~ country + continent,
                    nrow = 2, ncol = 6, width = 300, 
                    as_plotly = TRUE, 
                    plotly_args = list(dynamicTicks = T),
                    plotly_cfg = list(displayModeBar = F)
  )
# animation

data(gapminder, package = "gapminder")
gg <- ggplot(gapminder, aes(gdpPercap, lifeExp, color = continent)) +
  geom_point(aes(size = pop, frame = year, ids = country)) +
  scale_x_log10()
ggplotly(gg)

base <- gapminder %>%
  plot_ly(x = ~gdpPercap, y = ~lifeExp, size = ~pop, 
          text = ~country, hoverinfo = "text") %>%
  layout(xaxis = list(type = "log"))

base %>%
  add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
  animation_opts(1000, easing = "elastic", redraw = FALSE) %>%
  animation_button(
    x = 1, xanchor = "right", y = 0, yanchor = "bottom"
  ) %>%
  animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="red"))
  )

meanLife <- with(gapminder, tapply(lifeExp, INDEX = continent, mean))
gapminder$continent <- factor(
  gapminder$continent, levels = names(sort(meanLife))
)

base %>%
  add_markers(data = gapminder, frame = ~continent) %>%
  hide_legend() %>%
  animation_opts(frame = 1000, transition = 0, redraw = FALSE)
base %>%
  add_markers(color = ~continent, alpha = 0.2, showlegend = F) %>%
  add_markers(color = ~continent, frame = ~year, ids = ~country) %>%
  animation_opts(1000, redraw = FALSE)

# graphical queries
library(plotly)
mtcars %>%
  highlight_key(~cyl) %>%
  plot_ly(
    x = ~wt, y = ~mpg, text = ~cyl, mode = "markers+text", 
    textposition = "top", hoverinfo = "x+y"
  ) %>%
  highlight(on = "plotly_hover", off = "plotly_doubleclick")

library(crosstalk)

# generally speaking, use a "unique" key for filter, 
# especially when you have multiple filters!
tx <- highlight_key(txhousing)
gg <- ggplot(tx) + geom_line(aes(date, median, group = city))
filter <- bscols(
  filter_select("id", "Select a city", tx, ~city),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)

tx2 <- highlight_key(txhousing, ~city, "Select a city")
gg <- ggplot(tx2) + geom_line(aes(date, median, group = city))
select <- highlight(
  ggplotly(gg, tooltip = "city"), 
  selectize = TRUE, persistent = TRUE
)

bscols(filter, select)

tx <- highlight_key(txhousing)
widgets <- bscols(
  widths = c(12, 12, 12),
  filter_select("city", "Cities", tx, ~city),
  filter_slider("sales", "Sales", tx, ~sales),
  filter_checkbox("year", "Years", tx, ~year, inline = TRUE)
)
bscols(
  widths = c(4, 8), widgets, 
  plot_ly(tx, x = ~date, y = ~median, showlegend = FALSE) %>% 
    add_lines(color = ~city, colors = "black")
)
# dendrogram
hc <- hclust(dist(USArrests), "ave")
dend1 <- as.dendrogram(hc)
plot_dendro(dend1, height = 600) %>% 
  hide_legend() %>% 
  highlight("plotly_selected", persistent = TRUE, dynamic = TRUE)
demo("animation-tour-USArrests", package = "plotly")
