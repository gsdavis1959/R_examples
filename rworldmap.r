library(rworldmap)
d <- data.frame(
  country=c("GBR", "USA", "Germany", "Italy", "Norway"),
  value=c(-2, -1, 0, 1, 2))
n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="country")
n <- joinCountryData2Map(d, joinCode = "ISO3", 
                         nameJoinColumn = "country")

mapCountryData(n, nameColumnToPlot="value", mapTitle="World")


setwd('~/Data/Datasets')
population_records <- readLines("API_SP.POP.TOTL_DS2_en_csv_v2_10473719.csv")[-(1:4)]
population_data <- read.csv(text=population_records, header = TRUE)
head(population_data)
population_data$Growth.5.Year <- 
  ((population_data$X2014 - population_data$X2009) / population_data$X2014) * 100


n <- joinCountryData2Map(population_data, joinCode = "ISO3", 
                         nameJoinColumn = "Country.Code")

par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")
mapCountryData(n, nameColumnToPlot = "Growth.5.Year")
