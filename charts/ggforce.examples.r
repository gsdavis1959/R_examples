library(tidyverse)
library(ggforce)
library(nycflights13)
head(airports)

p <- airports %>%
  filter(lon < 0, tzone != "\\N") %>%
  ggplot(aes(lon, lat, color = tzone)) + 
  geom_point(show.legend = FALSE)  

p

p +
  geom_mark_rect() 

p + 
  geom_mark_rect(aes(label = tzone))

p + 
  geom_mark_rect(aes(label = tzone), show.legend = FALSE) +
  theme_void() 

p + 
  geom_mark_hull(aes(label = tzone)) +
  theme_void() 

p + 
  geom_mark_hull(aes(label = tzone, fill = tzone), show.legend = FALSE) +
  theme_void() 

p + 
  geom_mark_hull(aes(label = tzone, fill = tzone), show.legend = FALSE, expand = unit(3, "mm")) +
  theme_void() 

p + 
  geom_mark_hull(aes(label = tzone, fill = tzone), show.legend = FALSE, expand = unit(3, "mm")) +
  theme_no_axes() 

p +
  facet_zoom(xlim = c(-155, -160.5), ylim = c(19, 22.3))

p +
  facet_zoom(xy = tzone == "Pacific/Honolulu")

p +
  geom_mark_hull(aes(label = tzone, fill = tzone), show.legend = FALSE, expand = unit(3, "mm")) +
  theme_no_axes() +
  facet_zoom(x = tzone == "America/Los_Angeles")


p +
  geom_mark_hull(aes(fill = tzone), expand = unit(3, "mm")) +
  coord_cartesian(xlim = c(-130, -180), ylim = c(50, 75))  +
  geom_voronoi_segment()

prep_planes <- planes %>%
  filter(year > 1998, year < 2005) %>%
  filter(engine != "Turbo-shaft") %>%
  select(manufacturer, engine) %>%
  head(500)

prep_planes

prep_planes %>%
  gather_set_data(1:2)

prep_planes %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = engine)) 

prep_planes %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = engine)) +
  geom_parallel_sets_axes(axis.width = 0.1) +
  geom_parallel_sets_labels()

prep_planes %>%
  gather_set_data(1:2) %>%
  ggplot(aes(x, id = id, split = y, value = 1))  +
  geom_parallel_sets(aes(fill = engine), show.legend = FALSE, alpha = 0.3) +
  geom_parallel_sets_axes(axis.width = 0.1, color = "lightgrey", fill = "white") +
  geom_parallel_sets_labels(angle = 0) +
  theme_no_axes()

planes %>%
  count(engine) 
# doughnut
planes %>%
  count(engine) %>%
  ggplot() +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1, amount = n, fill = engine), alpha = 0.3, stat = "pie") 

planes %>%
  count(engine) %>%
  mutate(focus = ifelse(engine == "Turbo-jet", 0.2, 0)) %>%
  ggplot() +
  geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 0.7, r = 1, amount = n, fill = engine, explode = focus), alpha = 0.3, stat = "pie") +
  theme_no_axes()
