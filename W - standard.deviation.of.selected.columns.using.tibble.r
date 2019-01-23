library(tidyverse)
set.seed(733744)
n = 10
d = tibble(x1 = rnorm(n), x2 = rnorm(n),
           y1 = rnorm(n), y2 = rnorm(n),
           z1 = rnorm(n), z2 = rnorm(n))
d
d %>% 
  select(matches("x|y")) %>% 
  rowwise %>% 
  do(data.frame(., sd_xy = sd(unlist(.))))
