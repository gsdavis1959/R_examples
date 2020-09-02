#library
library(igraph)

# Create data
set.seed(10)
data <- matrix(sample(0:2, 25, replace=TRUE), nrow=5)
colnames(data) = rownames(data) = LETTERS[1:5]
data
# build the graph object
network <- graph_from_adjacency_matrix(data)

# plot it
plot(network)

# data
set.seed(1)
data <- matrix(sample(0:2, 15, replace=TRUE), nrow=3)
colnames(data) <- letters[1:5]
rownames(data) <- LETTERS[1:3]
data
# create the network object
network <- graph_from_incidence_matrix(data)

# plot it
plot(network)

