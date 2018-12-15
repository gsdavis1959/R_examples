library(ggplot2)

# Convert cyl column from a numeric to a factor variable
mtcars$cyl <- as.factor(mtcars$cyl)
head(mtcars)
p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot()

