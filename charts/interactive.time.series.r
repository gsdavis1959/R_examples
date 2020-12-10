library(quantmod)
start <- as.Date("2020-01-01")
end <- as.Date("2020-12-09")
getSymbols("^SP500TR", src = "yahoo", from = start, to = end)
getSymbols("C", src = "yahoo", from = start, to = end)
# extract closing prices
sp500 <- SP500TR$SP500TR.Close
C <- C$C.Close
# create single dataset
stocks <- cbind(sp500, C)
head(stocks)

# plot with ggplot
library(ggplot2)
autoplot(stocks)

# create plot using dygraph
library(dygraphs)
# basic chart
dygraph(stocks)
# customised chart
library(dplyr)
names(stocks)
stock_dyg <- dygraph(stocks, main = "SP500 and C Closing Price 1st Jan - 09th Dec 2020") %>%
  dySeries("SP500TR.Close", axis = "y2") %>% 
  dyAxis("y", 
         label = "SP500") %>%
  dyAxis("y2", 
         label = "C",
         valueRange = c(80, 120),
         independentTicks = TRUE) %>%
  dyRangeSelector(dateWindow = c("2020-01-01", "2020-12-09")) %>%
  dyRoller()
stock_dyg
# save as html
library(htmlwidgets)
saveWidget(stock_dyg, 
           "stock_dy.html",
           selfcontained = TRUE)