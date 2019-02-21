library(rworldmap)
d <- data.frame(
  country=c("GBR", "USA", "Germany", "Italy", "Norway"),
  value=c(-2, -1, 0, 1, 2))
n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="country")
mapCountryData(n, nameColumnToPlot="value", mapTitle="World")

n <- joinCountryData2Map(d, joinCode = "ISO3", 
                                   nameJoinColumn = "country")


