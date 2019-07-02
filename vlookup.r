library(tidyverse)
library(tibbletime)
setwd('~/Data/Datasets/Biology')
temp <- read_csv("temperature_schwartau.csv")
temp
flow <- read_csv("flow_schwartau.csv")
flow

flow <- flow %>%
  drop_na() %>%
  mutate(exit_flow = case_when( # get only departures
    flow >= 0 ~ flow
  )) %>%
  drop_na() %>% # drop all arriving rows
  arrange(timestamp) %>% # tble_time needs the timestamp to be ordered
  tbl_time(index = timestamp) %>% # convert to tble_time for ease of grouping by hour
  collapse_by(period = "hourly", clean = TRUE) %>% # "collapse" by hour
  group_by(timestamp) %>% # group by collapsed hourly column
  summarise(mean_exit_flow = mean(exit_flow)) # get the hourly average
flow

temp <- temp %>%
  drop_na() %>%
  arrange(timestamp) %>%
  tbl_time(index = timestamp) %>%
  collapse_by(period = "hourly", clean = TRUE) %>%
  group_by(timestamp) %>%
  summarise(mean_temp = mean(temperature))
temp

joined_table <- flow %>%
  left_join(temp, by = "timestamp")
joined_table

joined_table %>%
  ggplot(aes(x = mean_exit_flow, y = mean_temp)) +
  geom_point(alpha = .05) +
  labs(title = "Hourly Average Schwartau Beehive Activity", subtitle = "2017-01-01 through 2019-05-31", caption = "Data source: kaggle.com/se18m502/bee-hive-metrics") +
  xlab("Average Hourly Exit") +
  ylab("Average Hourly Temperature (C)") +
  theme_bw()

joined_table %>%
  ggplot(aes(x = timestamp, y = mean_exit_flow, color = mean_temp)) +
  geom_point() +
  labs(title = "Hourly Average Schwartau Beehive Activity", 
       subtitle = "2017-01-01 through 2019-05-31", 
       caption = "Data source: kaggle.com/se18m502/bee-hive-metrics",
       color = "Average\nTemp\n(C)") +
  xlab("Time") +
  ylab("Average Hourly Exit") +
  scale_colour_gradient(low = "black", high = "red") +
  theme_bw()
