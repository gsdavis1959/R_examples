library(tidyverse)

x <- c(NA, NA, 3)
y <- c(1, NA, 4)
z <- c(NA, 2, NA)
coalesce(x, y, z)

# 4 is ignored because their is a previous non-na in the column
