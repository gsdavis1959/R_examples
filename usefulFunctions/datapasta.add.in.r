library(datapasta)
library(tidyverse)

# copy table, use addins to execute datapasta (as df, datable, etc)

data.table::data.table(
    check.names = FALSE,
           Date = c("Aug 09, 2021","Aug 06, 2021",
                    "Aug 05, 2021","Aug 04, 2021","Aug 03, 2021"),
           Open = c(67.88, 69.14, 68.06, 70.32, 71.52),
           High = c(67.9, 70.18, 69.35, 70.81, 71.96),
            Low = c(65.15, 67.8, 67.61, 67.85, 69.19),
       `Close*` = c(66.85, 68.28, 69.09, 68.15, 70.56),
  `Adj.Close**` = c(66.85, 68.28, 69.09, 68.15, 70.56),
         Volume = c(556384, 416331, 416331, 582924, 509938)
)
