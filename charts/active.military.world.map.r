library(ggplot2)
library(leaflet)
library(rgdal)
library(maps)
library(tidyverse)
# -- r world

library(rworldmap)
library(classInt)
library(RColorBrewer)


setwd('~/Data/Datasets/Politics')
military <- read.csv('MilitaryData.csv', stringsAsFactors = FALSE)
str(military)
View(military)
names(military)


n <- joinCountryData2Map(military, joinCode = "NAME", 
                         nameJoinColumn = "Country")
df <- as.data.frame(n)
View(df)
names(df)
#getting class intervals using a ???jenks??? classification in classInt package
classInt <- classInt::classIntervals( military[["ActiveMilitary"]], n=5, style="jenks")
catMethod = classInt[["brks"]]
#getting a colour scheme from the RColorBrewer package
colourPalette <- RColorBrewer::brewer.pal(5,'Blues')
#calling mapCountryData with the parameters from classInt and RColorBrewer
mapParams <- mapCountryData(n,
                          nameColumnToPlot='ActiveMilitary',
                          addLegend=TRUE,
                          catMethod = catMethod,
                          colourPalette = colourPalette)
do.call(addMapLegend,
          c(mapParams,
                legendLabels="all",
                
                legendIntervals="data",
                legendMar = 2 ))




par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(n, nameColumnToPlot = "Per_1000_capita_total")
