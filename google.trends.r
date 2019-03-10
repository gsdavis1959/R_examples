# load libraries
devtools::install_github('PMassicotte/gtrendsR')

library(gtrendsR)
library(maps)
library(ggplot2)
library(lettercase)
library(viridis)
library(pals)
library(scico)
library(ggrepel)
library(tidyverse)

# create a theme
my_theme <- function() {
  theme_bw() +
    theme(panel.background = element_blank()) +
    theme(plot.background = element_rect(fill = "seashell")) +
    theme(panel.border = element_blank()) +                     # facet border
    theme(strip.background = element_blank()) +                 # facet title background
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
    theme(panel.spacing = unit(3, "lines")) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    theme(legend.background = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(legend.title = element_blank())
}

hurricanes <- gtrends(c("katrina","harvey"), time = "all", gprop = "web", geo = c("US"))
hurricanes %>% glimpse()

# plot the search
plot(hurricanes) + 
  my_theme() +
  geom_line(size = 0.5) 

# determine trends in a period of time
cycles <- gtrends(c("spring break","vacation"), 
                  time = "2008-01-01 2018-01-01", 
                  gprop = "web", 
                  geo = c("US"))
plot(cycles) +
  my_theme() +
  geom_line(size=0.5)

hurricanes %>% glimpse(78)

# look at hurricane Harvey
harvey <- gtrends(c("Harvey"), 
                  gprop = "web", 
                  time = "2017-08-18 2017-08-25", 
                  geo = c("US"))

# get searches by states
HarveyInterestByRegion <- as_tibble(harvey$interest_by_region)
HarveyInterestByRegion <- HarveyInterestByRegion %>% 
  dplyr::mutate(region = stringr::str_to_lower(location))

statesMap = ggplot2::map_data("state")

harveyMerged <- merge(statesMap, harvey, by = "region")
harveyMerged <- HarveyInterestByRegion %>% dplyr::left_join(x = ., 
                                                            y = statesMap, 
                                                            by = "region")

harveyMerged %>% dplyr::glimpse(78)

# plot on a us map
my_theme2 = function() {
  my_theme() +
    theme(axis.title = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.ticks = element_blank())
}

harveyMerged %>% 
  ggplot(aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, 
                   fill = log(hits)), 
               colour = "white") +
  scale_fill_gradientn(colours = rev(scico(15, 
                                           palette = "tokyo")[2:7])) +
  my_theme2() +
  ggtitle("Google search interest for Hurricane Harvey\nin each state from the week prior to landfall in the US") 

# search about guns
guns = gtrends(c("guns"), 
               gprop = "web", 
               time = "all", 
               geo = c("US"))

gunsInterestByRegion <- dplyr::as_tibble(guns$interest_by_region)
statesMap = map_data("state")
gunsInterestByRegion <- gunsInterestByRegion %>% 
  dplyr::mutate(region = stringr::str_to_lower(location))
gunsMerged <- gunsInterestByRegion %>% 
  dplyr::left_join(., statesMap, by = "region")
gunsMerged %>% glimpse(78)

# plot the guns search
gunsRegionLabels <- gunsMerged %>% 
  dplyr::select(region, 
                long, 
                lat) %>% 
  dplyr::group_by(region) %>% 
  dplyr::filter(!is.na(lat) | !is.na(long)) %>% 
  dplyr::summarise(lat = mean(range(lat)),
                   long = mean(range(long))) %>% 
  dplyr::arrange(region)
gunsRegionLabels %>% head()

gunsMerged %>% 
  ggplot2::ggplot(aes(x = long, y = lat)) +
  ggplot2::geom_polygon(aes(group = group, 
                            fill = log(hits))) +
  my_theme2() +
  ggplot2::coord_fixed(1.3) +
  ggplot2::scale_fill_distiller(palette = "Reds") +
  ggplot2::ggtitle("Google search interest for guns in each state")
