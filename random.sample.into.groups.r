setwd("~/Data/Datasets")
dat <- read.csv("random.prices.csv", stringsAsFactors = FALSE)
#a look at the data
head(dat)

library(randomizr)
Z <- complete_ra(N = 50, m = 3)
table(Z)

#sample 20 id's randomly
s1 <- (ids <- sample(unique(dat$Week),33))
s2 <- (ids <- sample(unique(dat$Week), 33))
s3 <- (ids <- sample(unique(dat$Week), 33))
s1
s2
s3

# The iris dataset is already loaded into your workspace
data("iris")
# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(iris)
shuffled <- iris[sample(n),]
head(shuffled)
# Split the data in train and test
g1 <- shuffled[1:30,]
g2 <- shuffled[31:60,]
g3 <- shuffled[61:90,]
g1
g2
g3
