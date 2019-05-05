library(inspectdf)
# some example data
data(starwars, package = "dplyr")
library(dplyr)
star_1 <- starwars %>% sample_n(50)
star_2 <- starwars %>% sample_n(50) %>% select(-1, -2)

# return tibble and visualisation of columns types
inspect_types(starwars, show_plot = TRUE)
inspect_types(star_1, star_2, show_plot = TRUE)
inspect_na(starwars, show_plot = TRUE)
inspect_na(star_1, star_2, show_plot = TRUE)
inspect_cor(starwars, show_plot = T)
inspect_cor(star_1, star_2, show_plot = TRUE)

# categorical variables
inspect_imb(starwars, show_plot = TRUE)
inspect_imb(star_1, star_2, show_plot = TRUE)

# histograms
inspect_num(starwars, show_plot = TRUE, breaks = 10)

inspect_num(starwars)$hist$birth_year
inspect_num(star_1, star_2, show_plot = TRUE)
inspect_cat(starwars, show_plot = T)
inspect_cat(starwars)$levels$hair_color
inspect_cat(star_1, star_2, show_plot = TRUE)
