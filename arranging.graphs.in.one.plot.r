# install.packages("devtools")
devtools::install_github("thomasp85/patchwork")
devtools::install_github("daattali/ggExtra", force = TRUE)

library(ggplot2)
library(patchwork)
library("ggExtra")

p1 <- ggplot(mtcars) + geom_point(aes(mpg, disp))
p2 <- ggplot(mtcars) + geom_boxplot(aes(gear, disp, group = gear))

p1 + p2

p3 <- ggplot(mtcars) + geom_smooth(aes(disp, qsec))
p4 <- ggplot(mtcars) + geom_bar(aes(carb))

(p1 | p2 | p3) /
  p4

set.seed(30)
df1 <- data.frame(x = rnorm(500, 50, 10), y = runif(500, 0, 50))
p1 <- ggplot(df1, aes(x, y)) + geom_point() + theme_bw()
p1
ggMarginal(p1)
ggMarginal(p1 + theme_bw(30) + ylab("Two\nlines"))

piris <- ggplot(iris, aes(Sepal.Length, Sepal.Width, colour = Species)) +
  geom_point()
ggMarginal(piris, groupColour = TRUE, groupFill = TRUE)

ggMarginal(p1, type = "histogram")

ggMarginal(p1, margins = "x", size = 2, type = "histogram",
           col = "blue", fill = "orange")

ggMarginal(p1, type = "histogram", xparams = list(binwidth = 1, fill = "orange"))

df3 <- data.frame(x = paste("Letter", LETTERS, sep = "_"),
                  y = seq_along(LETTERS))
p3 <- ggplot2::ggplot(df3, ggplot2::aes(x, y)) + ggplot2::geom_point()
p3 + rotateTextX()

plotCount(table(infert$education))

df4 <- data.frame("vehicle" = c("bicycle", "car", "unicycle", "Boeing747"),
                  "NumWheels" = c(2, 4, 1, 16))
plotCount(df4) + removeGridX()


