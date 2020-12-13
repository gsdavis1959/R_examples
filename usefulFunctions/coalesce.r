library(tidyverse)

x <- c(NA, NA, 3)
y <- c(1, NA, 4)
z <- c(NA, 2, NA)
coalesce(x, y, z)

# 4 is ignored because their is a previous non-na in the column

library(dplyr)
df <- read.csv('~/Data/Datasets/test-file.csv')
names(df)
df %>% 
  group_by(?..Name) %>%
  summarise_all(coalesce)

coalesce_all_columns <- function(df, group_vars = NULL) {
  
  if (is.null(group_vars)) {
    group_vars <- 
      df %>%
      purrr::keep(~ dplyr::n_distinct(.x) == 1L) %>% 
      names()
  }
  
  msk <- colnames(df) %in% group_vars
  same_df <- df[1L, msk, drop = FALSE]
  coal_df <- df[, !msk, drop = FALSE] %>%
    purrr::map_dfc(na.omit)
  
  cbind(same_df, coal_df)
}


df %>%
  group_by(?..Name) %>%
  do(coalesce_all_columns(.)) %>%
  ungroup()
#> # A tibble: 9 x 3
#>   Group Month Value
#>   <dbl> <chr> <chr>
#> 1     1 Jan   4    
#> 2     1 Feb   5    
#> 3     1 Mar   6    
#> 4     1 Jun   4    
#> 5     2 Jan   3    
#> 6     2 Mar   2    
#> 7     3 Feb   8    
#> 8     3 Mar   7    
#> 9     3 Jun   4


df %>%
  group_by(?..Name) %>%
  do(coalesce_all_columns(., "?..Name")) %>%
  ungroup()
#> # A tibble: 9 x 3
#>   Group Month Value
#>   <dbl> <chr> <chr>
#> 1     1 Jan   4    
#> 2     1 Feb   5    
#> 3     1 Mar   6    
#> 4     1 Jun   4    
#> 5     2 Jan   3    
#> 6     2 Mar   2    
#> 7     3 Feb   8    
#> 8     3 Mar   7    
#> 9     3 Jun   4

alpha <- c('a',NA,NA)
bravo <- c(NA,'b', NA)
cedric <- c(NA, NA,'c')
dave <-  c(NA,'dave','charlie')  

data <- tibble(alpha,bravo,cedric,dave)
data

coalesce(alpha, bravo)
coalesce(alpha, bravo, cedric)

data %>% 
  select(alpha:cedric) %>% 
  mutate(newcol = coalesce(alpha, bravo, cedric))
