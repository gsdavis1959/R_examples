library(dbscan)
library(fpc)
library(factoextra)

# Load the data 
# Make sure that the package factoextra is installed
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# Compute DBSCAN using fpc package
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
# Plot DBSCAN results
plot(db, df, main = "DBSCAN", frame = FALSE)

print(db)
db$cluster[sample(1:1089, 50)]

# Load the data
data("iris")
iris <- as.matrix(iris[, 1:4])

dbscan::kNNdistplot(iris, k =  4)
abline(h = 0.4, lty = 2)

set.seed(123)
# fpc package
res.fpc <- fpc::dbscan(iris, eps = 0.4, MinPts = 4)
# dbscan package
res.db <- dbscan::dbscan(iris, 0.4, 4)

all(res.fpc$cluster == res.db)

fviz_cluster(res.fpc, iris, geom = "point")

