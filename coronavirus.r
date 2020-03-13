# install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus", force = TRUE)
library(tidyverse)
library(coronavirus) 
library(lubridate)
library(ggmap)
library(maptools)
library(maps)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(mapview)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

data("coronavirus") 

head(coronavirus)
tail(coronavirus)

by_month <- coronavirus %>% mutate(month = month(date))

library(dplyr)

summary_df <- coronavirus %>% group_by(Country.Region, type, Lat, Long) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(-total_cases)

summary_df %>% head(20)

deaths <- summary_df %>% filter(type == 'death')

theme_set(theme_bw())

# Draw plot
ggplot(summary_df, aes(x=reorder(Country.Region, -total_cases), y = (total_cases/1000))) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  labs(title="Coronavirus Cases", 
       subtitle="Total Cases by Country", 
       caption="source: ") + 
  theme(axis.text.x = element_text(angle=90, vjust=0.1, hjust=1))

deaths <- rename(deaths, c("long"="Long", "lat"="Lat", 'region' = "Country.Region"))

library(tidyr)

coronavirus %>% 
  filter(date == max(date)) %>%
  select(country = Country.Region, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>%
  arrange(-confirmed)


map_world <- map_data("world")
names(map_world)
names(deaths)
map_world <- left_join(map_world, deaths, by = 'region')
map_world <- na.omit(map_world)

head(map_world)
tail(map_world)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
str(world)

ggplot(data = world) +
  geom_sf() +
  theme_classic() +
  geom_point(data = deaths, aes(x = long, y = lat,
                                colour = total_cases,
                                size = total_cases))

