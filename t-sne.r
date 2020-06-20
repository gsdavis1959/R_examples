library(Rtsne)
library(plotly)

iris_unique <- unique(iris) # remove duplicate rows 
head(iris_unique)

iris_matrix = as.matrix(iris_unique[,1:4]) # note: we can only pass in numeric columns
tsne_out <- Rtsne(iris_matrix) 
plot(tsne_out$Y,col=iris$Species) # graph is now generated

head(tsne_out)

df <- as.data.frame(tsne_out$Y)
iris_df <- slice(iris[2:150,])

fig <- plot_ly(data = df, x = ~V1, y = ~V2, type = 'scatter', text = iris_df$Species, color = ~iris_df$Species)

fig


