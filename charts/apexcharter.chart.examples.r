library(apexcharter)
library(tidyverse)
devtools::install_github("hadley/tidyverse", force = TRUE)

mtcars %>% 
  count(cyl) %>% 
  apex(type = "bar",
       mapping = aes(x = "cyl", y = n))
mtcars %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  as.data.frame() %>% 
  rownames_to_column("col") %>% 
  pivot_longer(cols = -col, names_to = "type") %>% 
  mutate(value = round(value,2)) %>% 
  apex(type = "heatmap",
       mapping = aes(x = col, y = type, fill = value)) 

df <- data.frame(Y=as.matrix(EuStockMarkets), date=time(EuStockMarkets))

df %>% 
  apex(type = "line",
       mapping = aes(x = date, y = Y.DAX)) 

data("economics", package = "ggplot2")
apex(data = economics, type = "line", mapping = aes(x = date, y = uempmed)) %>% 
  ax_stroke(width = 1)

data("mpg", package = "ggplot2")
n_manufac <- dplyr::count(mpg, manufacturer)

apex(data = n_manufac, type = "bar", mapping = aes(x = manufacturer, y = n))

