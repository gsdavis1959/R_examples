library(apexcharter)
library(tidyverse)


mtcars %>% 
  count(cyl) %>% 
  apex(type = "bar",
       mapping = aes(x = "cyl", y = n))