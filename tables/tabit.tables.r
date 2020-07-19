library(tabit)


ti_tab1(
  x = round(airquality$Solar.R, -2)
)

ti_tab1(
  x    = round(airquality$Solar.R, -2), 
  sort = FALSE
)

ti_tab1(
  x      = round(airquality$Solar.R, -2), 
  digits = 0
)

ti_tab1(
  x      = round(airquality$Solar.R, -2), 
  digits = 4
)

ti_tab1(
  x      = lapply(airquality, round, -2)
)

# get all counts
ti_tab1(x = airquality$Wind)$count

# get the highest percentage
tab <- ti_tab1(x = round(airquality$Solar.R, -2))
tab$pct[1]

# get percentage of NAs
tab$pct_all[is.na(tab$value)]
