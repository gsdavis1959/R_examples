devtools::install_github("Bart6114/sparklines")

library(quantmod); library(dplyr)

GOOG <-
  getSymbols("GOOG", src = 'yahoo', from = '2015-03-15', env = NULL) %>%
  as.data.frame %>%
  mutate(day_result = GOOG.Close - GOOG.Open)

GOOG_daily_result <-
  as.vector(GOOG$day_result)

GOOG_daily_result

library(sparklines)
sparkline(GOOG_daily_result, "line")
sparkline(GOOG_daily_result, "line", list(fillColor="white", height = "5in", width = "10in"))
sparkline(GOOG_daily_result, "bar", list(fillColor="red", height = "5in", width = "10in"))
sparkline(GOOG_daily_result, "tristate")
sparkline(GOOG_daily_result, "box")
