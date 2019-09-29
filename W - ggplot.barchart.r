library(ggplot2)

# Convert cyl column from a numeric to a factor variable
mtcars$cyl <- as.factor(mtcars$cyl)
head(mtcars)

# geom_bar is designed to make it easy to create bar charts that show
# counts (or sums of weights)
g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g + geom_bar()
g + geom_bar(aes(fill = drv))

# with means
ggplot(schools_long) + 
  geom_bar(aes(SCHOOL_YEAR, mean, fill = as.factor(name)), position = "dodge",
           stat = "summary", fun.y = "mean")
