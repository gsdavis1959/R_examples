library(apcluster)
## create two Gaussian clouds
cl1 <- cbind(rnorm(100,0.2,0.05),rnorm(100,0.8,0.06))
cl2 <- cbind(rnorm(100,0.7,0.08),rnorm(100,0.3,0.05))
x <- rbind(cl1,cl2)
x

## create negative distance matrix (default Euclidean)
sim1 <- negDistMat(x)
sim1

## compute similarities as squared negative distances
## (in accordance with Frey's and Dueck's demos)
sim2 <- negDistMat(x, r=2)
sim2
## compute RBF kernel
sim3 <- expSimMat(x, r=2)
sim3
## compute similarities as squared negative distances
## all samples versus a randomly chosen subset 
## of 50 samples (for leveraged AP clustering)
sel <- sort(sample(1:nrow(x), nrow(x)*0.25)) 
sim4 <- negDistMat(x, sel, r=2)
sim4

## example of leveraged AP using Minkowski distance with non-default
## parameter p
cl1 <- cbind(rnorm(150,0.2,0.05),rnorm(150,0.8,0.06))
cl2 <- cbind(rnorm(100,0.7,0.08),rnorm(100,0.3,0.05))
x <- rbind(cl1,cl2)

apres <- apclusterL(s=negDistMat(method="minkowski", p=2.5, r=2),
                    x, frac=0.2, sweeps=3, p=-0.2)
show(apres)

