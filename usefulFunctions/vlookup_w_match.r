library(tidyverse)

full_list <- structure(
  list(
    Ticker = c("AC", "AIC", "BBS", "BO", "C", "DF"),
    Long_Name = c("Ethanol -- CBOT", "DJ UBS Commodity Index -- CBOT", "South American Soybeans -- CBOT", "Soybean Oil -- CBT", "Corn -- CBT", "Dow Jones Industrial Average -- CBT")
  ),
  .Names = c("Ticker", "Long_Name"),
  row.names = c(NA, 6L),
  class = "data.frame"
)

replace <- structure(
  list(
    Type = c("F", "F", "F", "F", "F", "F"),
    Location = c("US", "US", "US", "US", "US", "US"),
    Symbol = c("BO", "C", "DF", "AIC", "AC", "BBS"),
    Month = c("V13", "U13", "U13", "U13", "U13", "U13")
  ),
  .Names = c("Type", "Location", "Symbol", "Month"),
  row.names = c(NA, 6L),
  class = "data.frame"
)

replace$Long_Name <- full_list$Long_Name[match(replace$Symbol,full_list$Ticker)]
replace

library(qdap)
replace$Long_Name <- lookup(replace$Symbol, full_list)
replace


library(data.table)
full_list <- data.table(full_list, key='Ticker')
replace <- data.table(replace, key='Symbol')
replace[full_list]


require(plyr)

colnames(replace)<-c("Type", "Location", "Ticker", "Month")

Full<-join(full_list, replace, by = "Ticker", type = "left", match = "all")
Full

