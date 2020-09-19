library(recommenderlab)

m <- matrix(sample(c(NA,0:5),100, replace=TRUE, prob=c(.7,rep(.3/6,6))),
            nrow=10, ncol=10, dimnames = list(
              user=paste('u', 1:10, sep=''),
              item=paste('i', 1:10, sep='')
            ))
m

## coerce into a realRatingMAtrix
r <- as(m, "realRatingMatrix")
r

## get some information
dimnames(r)
rowCounts(r) ## number of ratings per user
colCounts(r) ## number of ratings per item
colMeans(r) ## average item rating
nratings(r) ## total number of ratings
hasRating(r) ## user-item combinations with ratings

## histogram of ratings
hist(getRatings(r), breaks="FD")

## inspect a subset
image(r[1:5,1:5])

## coerce it back to see if it worked
as(r, "matrix")

## coerce to data.frame (user/item/rating triplets)
as(r, "data.frame")

## binarize into a binaryRatingMatrix with all 4+ rating a 1
b <- binarize(r, minRating=4)
b
as(b, "matrix")

rec <- Recommender(r, method = "POPULAR")
rec

getModel(rec)

pre <- predict(rec, r[1:2], n = 10)
pre
as(pre, "list")

## predict ratings for new users
pre <- predict(rec, r[1:2], type="ratings")
pre
as(pre, "matrix")[,1:2]


## create recommendations using user ids with ids 1..10 in the
## training data
pre <- predict(rec, 1:10 , data = r, n = 10)
pre
as(pre, "list")

