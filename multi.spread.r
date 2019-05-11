library(tidyverse)
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
#> The following object is masked from 'package:purrr':
#> 
#>     transpose

gen_cats <- function(x, N = 1000) {
  sample(x, N, replace = TRUE)
}

set.seed(101)
N <- 1000

income <- rnorm(N, 100, 50)

vars <- list(stratum = c(1:8),
             sex = c("M", "F"),
             race =  c("B", "W"),
             educ = c("HS", "BA"))

df <- as_tibble(map_dfc(vars, gen_cats))
df <- add_column(df, income)

df

## Data Table
data.table::setDT(df)
dt_wide <- data.table::dcast(df, sex + race + stratum ~ educ,
                             fun = list(mean, length),
                             value.var = "income")

head(dt_wide)


## Simple tidy summary
tv_wide1 <- df %>% group_by(sex, race, stratum, educ) %>%
  summarize(mean_inc = mean(income), N = n())

tv_wide1


## 1. gather()
tv_wide2 <- df %>% group_by(sex, race, stratum, educ) %>%
  summarize(mean_inc = mean(income), N = n()) %>%
  gather(variable, value, -(sex:educ))

tv_wide2

## 2. unite()
tv_wide2 <- df %>% group_by(sex, race, stratum, educ) %>%
  summarize(mean_inc = mean(income), N = n()) %>%
  gather(variable, value, -(sex:educ)) %>%
  unite(temp, educ, variable)

tv_wide2


## 3. spread()
tv_wide2 <- df %>% group_by(sex, race, stratum, educ) %>%
  summarize(mean_inc = mean(income), N = n()) %>%
  gather(variable, value, -(sex:educ)) %>%
  unite(temp, educ, variable) %>%
  spread(temp, value)

tv_wide2



multi_spread <- function(df, key, value) {
  # quote key
  keyq <- rlang::enquo(key)
  # break value vector into quotes
  valueq <- rlang::enquo(value)
  s <- rlang::quos(!!valueq)
  df %>% gather(variable, value, !!!s) %>%
    unite(temp, !!keyq, variable) %>%
    spread(temp, value)
}


## Final version
tv_wide3 <- df %>% group_by(sex, race, stratum, educ) %>%
  summarize(mean_inc = mean(income), N = n()) %>%
  multi_spread(educ, c(mean_inc, N))

tv_wide3



