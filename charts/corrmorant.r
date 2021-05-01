remotes::install_github("r-link/corrmorant")
library(corrmorant)
library(tidyverse)

corrmorant(drosera, style = "light")
corrmorant(drosera, style = "blue_red")

ggcorrm(drosera, mapping = aes(col = species, fill = species)) +
  lotri(geom_smooth(method = "lm")) +
  lotri(geom_point(alpha = 0.5)) +
  utri_corrtext(nrow = 2, squeeze = 0.6) +
  dia_names(y_pos = 0.15, size = 3) +
  dia_density(lower = 0.3, color = 1)
