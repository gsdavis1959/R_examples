library(tidyverse)

data <- tibble(a = 1:3, b = letters[c(1:2, NA)], c = 0.5 + 0:2)
data

# Insert
rows_insert(data, tibble(a = 4, b = "z"))

# Update
rows_update(data, tibble(a = 2:3, b = "z"))

rows_update(data, tibble(b = "z", a = 2:3), by = "a")

# Delete and truncate
rows_delete(data, tibble(a = 2:3))
rows_delete(data, tibble(a = 2:3, b = "b"))

# Variants: patch and upsert
rows_patch(data, tibble(a = 3, b = "z", c = 1))
