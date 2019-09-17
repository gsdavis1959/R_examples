m  <- as.matrix(dtm)
# # # m <- m[1:2, 1:3]
distMatrix <- dist(m, method="euclidean")
#print(distMatrix)
#distMatrix <- dist(m, method="cosine")
#print(distMatrix)

groups <- hclust(distMatrix,method="ward.D")
plot(groups, cex=0.9, hang=-1)
rect.hclust(groups, k=5)