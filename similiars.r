remotes::install_github("davidsjoberg/similiars")
library(similiars)
library(stringr)

fruit <- c('apple', 'bananna', 'peach')

x <- c("aple", "anana", "pech")
find_most_similiar_string(x, fruit)
