library(tidyverse)
library(inspectdf)
library("highcharter")
library(plotly)

schools <- read.csv('~/Data/Datasets/Education/school_diversity.csv', stringsAsFactors = FALSE)

head(schools)
summary(schools)
inspect_cor(schools)

schools_by_year <- schools %>%
    select(SCHOOL_YEAR, White, Black, Asian, Hispanic) %>%
    group_by(SCHOOL_YEAR) %>%
    inspect_cor()
schools_by_year%>% show_plot()  
names(schools)

schools %>% 
  group_by(SCHOOL_YEAR) %>%   
  skimr::skim_to_wide() %>% 
  knitr::kable()    

schools_long <- schools %>% 
  select(SCHOOL_YEAR, Asian, Black, Hispanic, White, Multi) %>%
  pivot_longer(
  cols = Asian:Multi,
  values_to = "mean",
  values_drop_na = TRUE
)

head(schools_long)

ggplot(schools_long) + 
  geom_bar(aes(SCHOOL_YEAR, mean, fill = as.factor(name)), position = "dodge",
           stat = "summary", fun.y = "mean")
