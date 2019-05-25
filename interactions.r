# source("https://install-github.me/jacob-long/interactions")
library(interactions)
fiti <- lm(mpg ~ hp * wt, data = mtcars)
sim_slopes(fiti, pred = hp, modx = wt, jnplot = TRUE)

interact_plot(fiti, pred = hp, modx = wt, interval = TRUE)
interact_plot(fiti, pred = hp, modx = wt, plot.points = TRUE)

fitiris <- lm(Petal.Length ~ Petal.Width * Species, data = iris)
interact_plot(fitiris, pred = Petal.Width, modx = Species, plot.points = TRUE)
