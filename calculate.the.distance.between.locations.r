library(dplyr)
# Example

setwd('~/Data/Datasets/Geography')
#1. Prepare data
newyork<-read.csv("NYC_Free_Public_WiFi_03292017.csv")
attach(newyork)
newyork = rename(newyork, index = 'NAME', lat = 'LAT', long = 'LON')
df <- newyork %>% 
  dplyr::select(index, lat, long) %>% 
  glimpse()
View(df)

library(geosphere)
library(maps)
data("us.cities")
head(us.cities)
df2 <- # filter rows
  us.cities %>% filter(country.etc == "NY") %>% glimpse()
View(df)
View(df2)


res = distm(df[c("long","lat")],df2[c("long","lat")])*0.0006213712
rownames(res) = df$index
colnames(res) = df2$name
View(res)


