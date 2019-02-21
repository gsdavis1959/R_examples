###########################################################################
## This script will replicate the figures (except the screenshots)
## and write them to files.
## The working directory should be set the the parent folder of this script.
###########################################################################

## install.packages(c("tmap", "tmaptools"))
library("tmap") # required version 1.11-1 or later
library("tmaptools") # required version 1.2-3 or later

data("World", "metro", package = "tmap")
head(World)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

#############################
## Figure 1
#############################

m1 <- tm_shape(World) +
    tm_polygons("income_grp", palette = "-Blues", 
      title = "Income class", contrast = 0.7, border.col = "grey30", id = "name") +
    tm_text("iso_a3", size = "AREA", col = "grey30", root = 3) +
  tm_shape(metro) +
    tm_bubbles("pop2010", col = "growth", border.col = "black",
      border.alpha = 0.5,
      breaks = c(-Inf, 0, 2, 4, 6, Inf) ,
      palette = "-RdYlGn",
      title.size = "Metro population (2010)", 
      title.col = "Annual growth rate (%)",
      id = "name",
      popup.vars = c("pop2010", "pop2020", "growth")) + 
  tm_format_World() + tm_style_gray(frame.lwd = 2)
save_tmap(m1, "bubble.png", width = 6.125, height = 3, scale = .75, dpi = 300, asp = 0, outer.margins = 0)
m1
#############################
## Figure 2
#############################

m0 <- tm_shape(metro) + 
  tm_bubbles(size = "pop2030") +
  tm_format_World() +
  tm_style_cobalt()
save_tmap(m0, "metro2030.png", width = 6.125, scale = .5, dpi = 300, outer.margins = 0)
m0

#############################
## Figure 3
#############################
m21 <- tm_shape(World) + tm_polygons(c("blue", "red")) + tm_layout(frame.lwd = 1.5)
m21
save_tmap(m21, "facets1.png", width = 6.125, height = 1.54, scale = .75, dpi = 300, outer.margins = 0)

#############################
## Figure 4
#############################
m22 <- tm_shape(World) + tm_polygons("red") + tm_facets(by = "continent", free.coords = FALSE)
m22
save_tmap(m22, "facets2.png", width = 6.125, height = 1.8, scale = .75, dpi = 300, outer.margins = 0)

#############################
## Figure 5
#############################
tm1 <- tm_shape(World) + tm_polygons()
tm2 <- tm_shape(metro) + tm_dots()
png("facets3.png", width = 6.125, height = 1.54, units = "in", res = 300)
  tmap_arrange(tm1, tm2, outer.margins = .01)
dev.off()

#############################
## Figure 6
#############################

tmap_mode("view")
m1

# a screenshot is taken from this interactive plot (named "view_metro4.jpg")

#############################
## Figure 7
#############################
data("land", "rivers", package = "tmap")
m2 <- tm_shape(land) +
  tm_raster("elevation", breaks = c(-Inf, 250, 500, 1000, 1500, 2000, 2500, 3000, 4000, Inf),  
            palette = terrain.colors(9), title = "Elevation (m)", auto.palette.mapping = FALSE) +
  tm_shape(rivers) + 
  tm_lines("lightblue", lwd = "strokelwd", scale = 1.5, legend.lwd.show = FALSE) +
  tm_shape(World, is.master = TRUE) +
  tm_borders("grey20", lwd = .5) +
  tm_grid(projection = "longlat", labels.size = 0.4, lwd = 0.25) +
  tm_text("name", size = "AREA") +
  tm_compass(position = c(0.08, 0.45), color.light = "grey90", size = 3) +
  tm_credits("Eckert IV projection", position = c("RIGHT", "BOTTOM")) +
  tm_layout(inner.margins = c(0.04, 0.04, 0.03, 0.02), 
            earth.boundary = TRUE) +
  tm_style_classic(bg.color = "lightblue",
                   space.color = "grey90") +
  tm_legend(position = c("left", "bottom"), 
            frame = TRUE,
            bg.color = "lightblue")
m2
save_tmap(m2, "classic.png", width = 6.125, scale = .7, dpi = 300, outer.margins = 0)


#############################
## Figure 8
#############################
m3 <- tm_shape(World, projection = "robin") +
  tm_polygons(c("HPI", "gdp_cap_est"), auto.palette.mapping = c(FALSE, TRUE), 
              palette = list("RdYlGn", "Purples"),
              style = c("pretty", "fixed"), n = 7, 
              breaks = list(NULL, c(0, 500, 2000, 5000, 10000, 25000, 50000, Inf)),
              title = c("Happy Planet Index", "GDP per capita")) +
  tm_format_World(inner.margins = 0.02, frame = FALSE) +
  tm_style_natural(earth.boundary = c(-180, 180, -87, 87))  +
  tm_legend(position = c("left", "bottom"), bg.color = "gray95", frame = TRUE) +
  tm_credits(c("", "Robinson projection"), position = c("RIGHT", "BOTTOM"))
m3  
save_tmap(m3, "world_facets2.png", width = 5, scale = .7, dpi = 300, outer.margins = 0)


#############################
## Figure 9
#############################
library("readxl")
library("grid")

# function to obtain Food Environment Atlas data (2014)
get_food_envir_data <- function() {
  dir <- tempdir()
  download.file("https://www.ers.usda.gov/webdocs/DataFiles/48731/February2014.xls?v=41688", destfile = file.path(dir, "DataDownload.xls"), mode = "wb")
  read_excel(file.path(dir, "DataDownload.xls"), sheet = "HEALTH")
}

# function to obtain US county shape
get_US_county_2010_shape <- function() {
  dir <- tempdir()
  download.file("http://www2.census.gov/geo/tiger/GENZ2010/gz_2010_us_050_00_20m.zip", destfile = file.path(dir, "gz_2010_us_050_00_20m.zip"))
  unzip(file.path(dir, "gz_2010_us_050_00_20m.zip"), exdir = dir)
  US <- read_shape(file.path(dir, "gz_2010_us_050_00_20m.shp"))
  levels(US@data$NAME) <- iconv(levels(US@data$NAME), from = "latin1", to = "utf8")
  US
}

# obtain Food Environment Atlas data
FEA <- get_food_envir_data()

# obtain US county shape
US <- get_US_county_2010_shape()

us1 <- qtm(US)

save_tmap(us1, "US1.png", scale = .5, width = 6.125, asp = 0, outer.margins = 0)

#############################
## Figure 10
#############################
US$FIPS <- paste0(US$STATE, US$COUNTY)

# append data to shape
US <- append_data(US, FEA, key.shp = "FIPS", key.data = "FIPS", ignore.duplicates = TRUE)

unmatched_data <- over_coverage()
str(unmatched_data)

tmap_mode("view")
qtm(US, fill = "PCT_OBESE_ADULTS10")

# a screenshot is taken from this interactive plot (named "US2b.jpg")


#############################
## Figure 11
#############################

US_cont <- US %>% 
  subset(!STATE %in% c("02", "15", "72")) %>% 
  simplify_shape(0.2) 

US_AK <- US %>% 
  subset(STATE == "02") %>% 
  simplify_shape(0.2) 

US_HI <- US %>% 
  subset(STATE == "15") %>% 
  simplify_shape(0.2) 

# create state boundaries
US_states <- US_cont %>% 
  aggregate_map(by = "STATE")

# contiguous US
m_cont <- tm_shape(US_cont, projection = 2163) +
  tm_polygons("PCT_OBESE_ADULTS10", border.col = "gray50", border.alpha = .5, title = "", showNA = TRUE) +
  tm_shape(US_states) +
  tm_borders(lwd = 1, col = "black", alpha = .5) +
  tm_credits("Data @ Unites States Department of Agriculture\nShape @ Unites States Census Bureau", position = c("right", "bottom")) +
  tm_layout(title = "2010 Adult Obesity by County, percent", 
            title.position = c("center", "top"), 
            legend.position = c("right", "bottom"), 
            frame = FALSE, 
            inner.margins = c(0.1, 0.1, 0.05, 0.05))

# Alaska inset
m_AK <- tm_shape(US_AK, projection = 3338) +
  tm_polygons("PCT_OBESE_ADULTS10", border.col = "gray50", border.alpha = .5, breaks = seq(10, 50, by = 5)) +
  tm_layout("Alaska", legend.show = FALSE, bg.color = NA, title.size = 0.8, frame = FALSE)

# Hawaii inset
m_HI <- tm_shape(US_HI, projection = 3759) +
  tm_polygons("PCT_OBESE_ADULTS10", border.col = "gray50", border.alpha = .5, breaks = seq(10, 50, by = 5)) +
  tm_layout("Hawaii", legend.show = FALSE, bg.color = NA, title.position = c("LEFT", "BOTTOM"), title.size = 0.8, frame = FALSE)

# specify viewports for Alaska and Hawaii
vp_AK <- viewport(x = 0.15, y = 0.15, width = 0.3, height = 0.3)
vp_HI <- viewport(x = 0.4, y = 0.1, width = 0.2, height = 0.1)

# save map
tmap_mode("plot")
save_tmap(m_cont, "USchoro.png", scale = 0.7, width = 6.125, outer.margins = 0,
          insets_tm = list(m_AK, m_HI), 
          insets_vp = list(vp_AK, vp_HI))


#############################
## Figure 12a
#############################
library("sp")
library("rnaturalearth")

# functions to obtain crimes data
get_crimes_data <- function(path) {
  stopifnot(file.exists(path), ("crimes_in_Greater_London_2015-10.zip" %in% list.files(path)))
  tmp_dir <- tempdir()
  unzip(file.path(path, "crimes_in_Greater_London_2015-10.zip"), exdir = tmp_dir)
  rbind(read.csv(file.path(tmp_dir, "2015-10-city-of-london-street.csv")),
        read.csv(file.path(tmp_dir, "2015-10-metropolitan-street.csv")))
}

# please download the file "crimes_in_Greater_London_2015-10.zip" (available on https://www.jstatsoft.org as a supplement of this paper), and change the path argument below to the location of the downloaded file:
crimes <- get_crimes_data(path = "./")

# create SpatialPointsDataFrame of known locations
crimes <- crimes[!is.na(crimes$Longitude) & !is.na(crimes$Latitude), ]
coordinates(crimes) <- ~ Longitude + Latitude

# set map projection to British National Grid
crimes <- set_projection(crimes, current.projection = "longlat", projection = 27700)

c1 <- qtm(crimes)
save_tmap(c1, "crimes1.png", scale = .6, width = 3, units = "in", outer.margins = 0)

#############################
## Figure 12b
#############################
crimes_osm <- read_osm(crimes)
c2 <- qtm(crimes_osm) + qtm(crimes, symbols.col = "red", symbols.size = 0.5)
save_tmap(c2, "crimes2.jpg", scale = .6, width = 3, units = "in", outer.margins = 0)


#############################
## Figure 13
#############################
c3 <- qtm(crimes_osm, raster.saturation = 0, raster.alpha = 1) + 
  qtm(crimes, symbols.col = "Crime.type", symbols.size = 0.5) +
  tm_legend(outside = TRUE)
save_tmap(c3, "crimes3.png", scale = .8, width = 5, height = 4, units = "in", outer.margins = 0)


#############################
## Figure 14
#############################
regions <- ne_download(scale = "large", type = "states", category = "cultural")
london <- regions[which(regions$region == "Greater London"),]
london <- set_projection(london, projection = 27700)

# remove crimes outside Greater London
crimes_london <- crop_shape(crimes, london, polygon =  TRUE)

c3b <- qtm(crimes_london, dots.alpha = 0.1) +
  tm_shape(london) + 
  tm_borders()
save_tmap(c3b, "crimes3b.png", scale = .7, width = 6.125, units = "in", outer.margins = 0)

#############################
## Figure 15
#############################
crime_densities <- smooth_map(crimes_london, bandwidth = 0.5, breaks = c(0, 50, 100, 250, 500, 1000), cover = london)

# download rivers, and get Thames shape
rivers <- ne_download(scale = "large", type = "rivers_lake_centerlines", category = "physical")
thames <- crop_shape(rivers, london)

c4 <- tm_shape(crime_densities$polygons) +
  tm_fill(col = "level", palette = "YlOrRd", title = expression("Crimes per " * km^2)) + 
  tm_shape(london) + tm_borders() +
  tm_shape(thames) + tm_lines(col = "steelblue", lwd = 4) +
  tm_compass(position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) + 
  tm_style_gray(title = "Crimes in Greater London\nOctober 2015")
save_tmap(c4, "crimes4.png", scale = .7, width = 6.125, units = "in", outer.margins = 0)


#############################
## Figure 16
#############################
london_city <- london[london$name == "City",]
crimes_city <- crop_shape(crimes_london, london_city, polygon = TRUE)
london_osm <- read_osm(london_city, type = "stamen-watercolor", zoom = 13)

c5 <- qtm(london_osm) +
  tm_shape(crimes_city) +
  tm_dots(size = .2) +
  tm_facets("Crime.type", free.coords = FALSE)
save_tmap(c5, "crimes5.png", scale = 1, width = 6.125, asp = NA, outer.margins = 0)


#############################
## Figure 17
#############################
crime_lookup <- c("Anti-social behaviour" = 2, 
                  "Bicycle theft" = 1, 
                  "Burglary" = 1,
                  "Criminal damage and arson" = 2,
                  "Drugs" = 6, 
                  "Other crime" = 7,
                  "Other theft" = 1, 
                  "Possession of weapons" = 3, 
                  "Public order" = 2, 
                  "Robbery" = 1, 
                  "Shoplifting" = 1,
                  "Theft from the person" = 1,
                  "Vehicle crime" = 4,
                  "Violence and sexual offences" = 5)
crime_categories <- c("Property Crime",
                      "Criminal damage and anti-social behaviour",
                      "Possession of weapons",
                      "Vehicle crime",
                      "Violence and sexual offences",
                      "Drugs",
                      "Other crime")
crimes_city$Crime.group <- factor(crime_lookup[crimes_city$Crime.type], labels = crime_categories)

tmap_mode("view")
tm_shape(crimes_city) +
  tm_dots(jitter = 0.2, col = "Crime.group", palette = "Dark2", popup.vars = TRUE) +
  tm_view(alpha = 1,
    basemaps = "Esri.WorldTopoMap")

# a screenshot is taken from this interactive plot (named "view_crimes2.JPG")
