library(tidyverse)
library(tsibble)

holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

library(feasts)
holidays %>% autoplot(Trips)

holidays %>% gg_season(Trips)
holidays %>% gg_subseries(Trips)

holidays %>% ACF(Trips)
holidays %>% ACF(Trips) %>% autoplot()
holidays %>% filter(State=="Tasmania") %>% gg_lag(Trips, geom="point")
holidays %>% filter(State=="Tasmania") %>% gg_tsdisplay(Trips)

# decomposition
holidays %>% STL(Trips) %>% autoplot()


library(lubridate)
tsibbledata::vic_elec %>%
  filter(yearmonth(Date) >= yearmonth("2014 Oct")) %>%
  STL(Demand ~ trend(window=77) + season(window="periodic")) %>%
  autoplot()

tsibbledata::vic_elec %>%
  filter(yearmonth(Date) >= yearmonth("2014 Oct")) %>%
  STL(
    Demand ~ trend(window=77) +
      season("week", window="periodic")
  ) %>%
  autoplot()
