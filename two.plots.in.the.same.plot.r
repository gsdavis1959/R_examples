library(ggplot2)

set.seed(42)
n <- 1000
x <- runif(n) * 3
y <- x * sin(1/x) + rnorm(n) / 25
df <- data.frame(x = x, y = y)

p1 <- ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  theme_bw()
p1

p2 <- ggplot(df, aes(x, y)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(limits = c(0, 0.5)) +
  scale_y_continuous(limits = c(-0.3, 0.6)) +
  theme_bw()
p2
# put two plots in the same graph
p1 + annotation_custom(ggplotGrob(p2), xmin = 1, xmax = 3, 
                       ymin = -0.3, ymax = 0.6)
# us map
library(ggmap)

us_bbox <- c(left = -125, bottom = 25, right = -55, top = 50)
us_main_map <- get_stamenmap(us_bbox, zoom = 5, maptype = "terrain")
p_main <- ggmap(us_main_map)
p_main

alaska_bbox <- c(left = -180, bottom = 50, right = -128, top = 72)
alaska_map <- get_stamenmap(alaska_bbox, zoom = 5, maptype = "terrain") 
p_alaska <- ggmap(alaska_map) + 
  labs(title = "Alaska") +
  theme(axis.title = element_blank(), 
        axis.text  = element_blank(),
        axis.ticks = element_blank())
p_alaska

hawaii_bbox <- c(left = -160, bottom = 18.5, right = -154.5, top = 22.5)
hawaii_map <- get_stamenmap(hawaii_bbox, zoom = 6, maptype = "terrain") 
p_hawaii <- ggmap(hawaii_map) + 
  labs(title = "Hawaii") +
  theme(axis.title = element_blank(), 
        axis.text  = element_blank(),
        axis.ticks = element_blank())
p_hawaii

library(grid)
p_main +
  inset(ggplotGrob(p_alaska), xmin = -76.7, xmax = -66.7, ymin = 26, ymax = 35) +
  inset(ggplotGrob(p_hawaii), xmin = -66.5, xmax = -55.5, ymin = 26, ymax = 35)
