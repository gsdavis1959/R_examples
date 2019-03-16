setwd('~/Data/Datasets/Geography')
#1. Prepare data
newyork<-read.csv("NYC_Free_Public_WiFi_03292017.csv")
attach(newyork)
newyorkdf<-data.frame(newyork$LAT,newyork$LON)
View(newyork)

#2. Determine number of clusters
wss <- (nrow(newyorkdf)-1)*sum(apply(newyorkdf,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(newyorkdf,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#3. K-Means Cluster Analysis
set.seed(20)
fit <- kmeans(newyorkdf, 11) # 11 cluster solution
# get cluster means
aggregate(newyorkdf,by=list(fit$cluster),FUN=mean)
# append cluster assignment
newyorkdf <- data.frame(newyorkdf, fit$cluster)
View(newyorkdf)
newyorkdf$fit.cluster <- as.factor(newyorkdf$fit.cluster)
library(ggplot2)
ggplot(newyorkdf, aes(x=newyork.LON, y=newyork.LAT, color = newyorkdf$fit.cluster)) + geom_point()



library(leaflet)
leaflet() %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 12)
leaflet(newyorkdf) %>% addTiles() %>% addMarkers(~newyork.LON, ~newyork.LAT,
  clusterOptions = markerClusterOptions()
)
# Show first 20 rows from the `quakes` dataset
leaflet(data = newyorkdf) %>% addTiles() %>%
  addMarkers(~newyork.LON, ~newyork.LAT, popup = ~as.character(newyorkdf$fit.cluster), color = ~as.character(newyorkdf$fit.cluster))

library(maps)
mapStates = map("state", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)


mapWorld = map("world", fill = TRUE, plot = FALSE)
leaflet(data = mapWorld) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))
