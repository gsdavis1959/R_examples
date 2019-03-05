# Libraries
library(tidyverse)

# Get the world polygon and extract UK
library(maps)

UK <- map_data("world") %>% filter(region=="UK")

# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data=world.cities %>% filter(country.etc=="UK")
View(data)

USA <- map_data("state") %>% filter(region=="USA")
View(USA)
# Get a data frame with longitude, latitude, and size of bubbles (a bubble = a city)
data_us=world.cities %>% filter(country.etc=="USA")
View(data_us)
names(data_us)

states <- map_data("state")
usamap <- ggplot(states, aes(long, lat, group = group)) +
  geom_polygon(fill = "white", colour = "black")
usamap

# Left chart
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat)) +
  theme_void() + ylim(50,59) + coord_map() 

map <- borders("usa", colour="black", fill="white", size = 1) #map USA continent

ggplot() +
  geom_polygon(data = USA, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data_us, aes(x=long, y=lat)) +
  theme_void() + coord_map()

# Second graphic with names of the 10 biggest cities
library(ggrepel)
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + ylim(50,59) + coord_map() +
  theme(legend.position="none")
# USA

ggplot() +
  geom_polygon(data = states, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data_us, aes(x=long, y=lat, alpha=pop)) +
  geom_text_repel( data=data_us %>% arrange(pop) %>% tail(10), aes(x=long, y=lat, label=name), size=5) +
  geom_point( data=data_us %>% arrange(pop) %>% tail(10), aes(x=long, y=lat), color="red", size=3) +
  theme_void() + coord_map() +
  theme(legend.position="none") 




library(viridis)

# Left: use size and color
ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data, aes(x=long, y=lat, size=pop, color=pop)) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() 

# Center: reorder your dataset first! Big cities appear later = on top
data %>%
  arrange(pop) %>% # This reorder your data frame
  mutate( name=factor(name, unique(name))) %>%
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( aes(x=long, y=lat, size=pop, color=pop), alpha=0.9) +
  scale_size_continuous(range=c(1,12)) +
  scale_color_viridis(trans="log") +
  theme_void() + ylim(50,59) + coord_map() + theme(legend.position="none")

# Right: just use arrange(desc(pop)) instead

mybreaks=c(0.02, 0.04, 0.08, 1, 7)
data %>%
  arrange(pop) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate(pop=pop/1000000) %>%
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(  aes(x=long, y=lat, size=pop, color=pop, alpha=pop), shape=20, stroke=FALSE) +
  scale_size_continuous(name="Population (in M)", trans="log", range=c(1,12), breaks=mybreaks) +
  scale_alpha_continuous(name="Population (in M)", trans="log", range=c(0.1, .9), breaks=mybreaks) +
  scale_color_viridis(option="magma", trans="log", breaks=mybreaks, name="Population (in M)" ) +
  theme_void() + ylim(50,59) + coord_map() + 
  guides( colour = guide_legend()) +
  ggtitle("The 1000 biggest cities in the UK") +
  theme(
    legend.position = c(0.85, 0.8),
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.title = element_text(size= 16, hjust=0.1, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  )

# Easy to make it interactive! Doesn't work
library(plotly)

# plot
p=data %>%
  
  arrange(pop) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate( mytext=paste("City: ", name, "\n", "Population: ", pop, sep="")) %>%  # This prepare the text displayed on hover.
  
  # Makte the static plot calling this text:
  ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point(aes(x=long, y=lat, size=pop, color=pop, text=mytext, alpha=pop) ) +
  
  scale_size_continuous(range=c(1,15)) +
  scale_color_viridis(option="inferno", trans="log" ) +
  scale_alpha_continuous(trans="log") +
  theme_void() +
  ylim(50,59) +
  coord_map() +
  theme(legend.position = "none")

p=ggplotly(p, tooltip="text")
