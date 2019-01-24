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


