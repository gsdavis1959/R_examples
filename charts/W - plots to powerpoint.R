library("tidyverse")
library("officer")
library("rvg")
setwd("~/Data/Datasets")
time_use <- rio::import("clean_data.csv")


time_use <- time_use %>%
  filter(population %in% c("Male", "Female")) %>%
  filter(activities %in% c("Personal care", "Sleep", "Eating", 
                           "Employment", "Household and family care")) %>%
  group_by(day) %>%
  nest()
time_use
time_use$data[1]

my_plots <- time_use %>%
  mutate(plots = map2(.y = day, .x = data, ~ggplot(data = .x) + theme_minimal() +
                        geom_col(aes(y = time_in_minutes, x = activities, fill = population), 
                                 position = "dodge") +
                        ggtitle(.y) +
                        ylab("Time in minutes") +
                        xlab("Activities")))

my_plots
my_plots$plots[1]

# function to send plots to ppt
create_pptx <- function(plot, path){
  if(!file.exists(path)) {
    out <- read_pptx()
  } else {
    out <- read_pptx(path)
  }
  
  out %>%
    add_slide(layout = "Title and Content", master = "Office Theme") %>%
    ph_with_vg(code = print(plot), type = "body") %>% 
    print(target = path)
}
map(my_plots$plots, create_pptx, path = "test.pptx")
