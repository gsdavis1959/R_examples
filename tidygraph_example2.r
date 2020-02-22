library(tidygraph)
create_ring(10)
plot(create_ring(8))


iris_clust <- hclust(dist(iris[1:4]))
head(iris_clust)
iris_tree <- as_tbl_graph(iris_clust)
iris_tree

