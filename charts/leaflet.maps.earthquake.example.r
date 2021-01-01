library(leaflet)
library(rgdal)
library(dplyr)

japan_lat <- 138.129731
japan_lon <- 38.0615855

data <- read_csv('C:/Users/gsdav/Documents/Data/Datasets/Japan earthquakes 2001 - 2018.csv')

strong <- data %>%
  filter(mag >= 6)

leaflet() %>%
  setView(lng = japan_lat, lat = japan_lon, zoom = 6) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = strong,
    radius = strong$mag
  )

# change marker size based on magnitude
leaflet() %>%
  setView(lng = japan_lat, lat = japan_lon, zoom = 6) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = strong,
    radius = sqrt(10^strong$mag) * 2
  )

# add circles
leaflet() %>%
  setView(lng = japan_lat, lat = japan_lon, zoom = 6) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = strong,
    radius = sqrt(10^strong$mag) * 2,
    color = "#F60D1D",
    fillColor = "#F60D1D",
    fillOpacity = 0.25
  )


# add pop ups
leaflet() %>%
  setView(lng = japan_lat, lat = japan_lon, zoom = 6) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addCircles(
    data = strong,
    radius = sqrt(10^strong$mag) * 2,
    color = "#F60D1D",
    fillColor = "#F60D1D",
    fillOpacity = 0.25,
    popup = paste0(
      "<strong>Time: </strong>", strong$time, "<br>",
      "<strong>Magnitude: </strong>", strong$mag, "<br>",
      "<strong>Depth (km): </strong>", strong$depth, "<br>",
      "<strong>Place: </strong>", strong$place, "<br>"
    )
  )
