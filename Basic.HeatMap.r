library(apexcharter)
library(tidyverse)

mtcars %>% 
  select_if(is.numeric) %>% 
  cor() %>% 
  as.data.frame() %>% 
  rownames_to_column("col") %>% 
  pivot_longer(cols = -col, names_to = "type") %>% 
  mutate(value = round(value,2)) %>% 
  apex(type = "heatmap",
       mapping = aes(x = col, y = type, fill = value)) 
