library(tidyverse)
dat = as.data.frame(esoph)
dat

# I want to create tertiles for each variable in the dataset
# and want to exclude the ncases and ncontrols from the computation.
dat %>% 
  mutate_at(list(tertile = ~ntile(., 3)), .vars = vars(ends_with("gp"), -starts_with("nc")))
# decile
dat %>% 
  mutate_at(list(decile = ~ntile(., 10)), .vars = vars(ends_with("gp"), -starts_with("nc")))
# quartile
dat %>% 
  mutate_at(list(quartile = ~ntile(., 4)), .vars = vars(ends_with("gp"), -starts_with("nc")))

# standardize the ncases and ncontrols variables
dat %>% 
  mutate_at(list(sd = ~./sd(.)), .vars = vars(-ends_with("gp"), starts_with("nc")))
