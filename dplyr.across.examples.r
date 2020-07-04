library(dplyr)

mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(c("mpg", "hp"), mean))

mtcars %>% 
  group_by(cyl) %>% 
  summarise(across(c("mpg", "hp"), list(mean = mean, median = median, sd = sd, sum = sum))) 

# summarize across columns

WorldPhones %>% 
  as.data.frame() %>% 
  rowwise() %>% 
  mutate(mean = mean(c_across(N.Amer:Mid.Amer), na.rm = TRUE))


starwars %>% 
  rowwise() %>% 
  mutate(
    stuff_they_own = length(c_across(c("vehicles", "starships")))
  ) %>% 
  select(name, stuff_they_own) 
