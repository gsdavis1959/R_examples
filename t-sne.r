library(Rtsne)

iris_unique <- unique(iris) # remove duplicate rows 
head(iris_unique)

iris_matrix = as.matrix(iris_unique[,1:4]) # note: we can only pass in numeric columns
tsne_out <- Rtsne(iris_matrix) 
plot(tsne_out$Y,col=iris$Species) # graph is now generated

