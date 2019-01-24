data(iris)
head(iris)
for (row in 1:nrow(iris)) {
  Part1 <- iris[row, "Sepal.Length"]
  Part2  <- iris[row, "Petal.Length"]
  
  
  print(paste("On Sepal length", Part1, 
                "the petal length was", Part2))

}

