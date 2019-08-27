library(purrr)
data(mtcars)
mtcars %>% split(mtcars$carb) %>% map(summary)

