library("ggspatial")
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
# black and white map with compass and distance legend
ggplot(data = world) +
  geom_sf() +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97))

## Scale on map varies by more than 10%, scale bar may be inaccurate
# with country names
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world) +
  geom_sf() +
  geom_text(data= world_points,aes(x=X, y=Y, label=name),
            color = "darkblue", fontface = "bold", check_overlap = FALSE) +
  annotate(geom = "text", x = -90, y = 26, label = "Gulf of Mexico", 
           fontface = "italic", color = "grey22", size = 6) +
  coord_sf(xlim = c(-102.15, -74.12), ylim = c(7.65, 33.97), expand = FALSE)


ggplot(data = world) + 
  geom_sf(fill= 'antiquewhite') + 
  geom_text(data= world_points,aes(x=X, y=Y, label=name), 
  color = 'darkblue', fontface = 'bold', 
  check_overlap = FALSE) + annotate(geom = 'text', 
  x = -300, y = 400, label = 'Gulf of Mexico', 
  fontface = 'italic', color = 'grey22', size = 6) + 
  annotation_scale(location = 'bl', width_hint = 0.5) + 
  annotation_north_arrow(location = 'bl', 
  which_north = 'true', pad_x = unit(0.75, 'in'), 
  pad_y = unit(0.5, 'in'), style = north_arrow_fancy_orienteering) + 
  coord_sf(xlim = c(-102.15, -74.12), 
  ylim = c(7.65, 33.97), expand = FALSE) + 
  xlab('Longitude') + ylab('Latitude') + 
  ggtitle('Map of the Gulf of Mexico and the Caribbean Sea') + 
  theme(panel.grid.major = element_line(color = gray(.5), linetype = 'dashed',
  size = 0.5), panel.background = element_rect(fill = 'aliceblue'))
