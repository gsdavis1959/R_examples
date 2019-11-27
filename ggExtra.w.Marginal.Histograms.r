library(ggExtra)
library(ggplot2)
library(gridExtra)
data("mtcars")
names(mtcars)
# create dataset with 1000 normally distributed points
df <- data.frame(x = rnorm(1000, 50, 10), y = rnorm(1000, 50, 10))
# create a ggplot2 scatterplot
p <- ggplot(mtcars, aes(mpg, hp)) + geom_point() + theme_classic()
# add marginal histograms
ggExtra::ggMarginal(p, type = "histogram")

pMain <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
pTop <- ggplot(mtcars, aes(x = wt)) +
  geom_histogram()
pRight <- ggplot(mtcars, aes(x = mpg)) +
  geom_histogram() + coord_flip()
pEmpty <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_blank() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        line = element_blank(),
        panel.background = element_blank())

grid.arrange(pTop, pEmpty, pMain, pRight,
             ncol = 2, nrow = 2, widths = c(3, 1), heights = c(1, 3))


ggExtra::ggMarginal(data = mtcars, x = "wt", y = "mpg")  
ggExtra::ggMarginal(ggplot(mtcars, aes(wt, mpg)) + geom_point())
