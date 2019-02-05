library(ggplot2)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, size=Petal.Length)) + 
  geom_point(alpha=0.6) +
  scale_size_continuous(breaks = c(1,3,6))
