library("recommenderlab")	 	 
library(ggplot2)
library(data.table)
library(reshape2)

setwd('C:/Users/gsdav/Documents/Data/Datasets/recommender/ml-latest-small')

movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")
str(movie_data)

summary(movie_data)
head(movie_data)

summary(rating_data)
head(rating_data)

movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
library(data.table)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) #DataFlair
colnames(movie_genre2) <- c(1:10)
list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10329,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) #Author DataFlair
    genre_mat1[index+1,gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)
str(movie_data)



SearchMatrix <- cbind(movie_data[1:10328,1:2], genre_mat2[])
head(SearchMatrix)


ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
ratingMatrix


recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)

similarity_mat <- similarity(ratingMatrix[1:4, ],
                             method = "cosine",
                             which = "users")
as.matrix(similarity_mat)
image(as.matrix(similarity_mat), main = "User's Similarities")


movie_similarity <- similarity(ratingMatrix[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies similarity")


rating_values <- as.vector(ratingMatrix@data)
unique(rating_values) # extracting unique ratings

Table_of_Ratings <- table(rating_values) # creating a count of movie ratings
Table_of_Ratings


movie_views <- colCounts(ratingMatrix) # count views for each movie
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) # create dataframe of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] # sort by number of views
str(table_views)

table_views$title <- NA
for (index in 1:9724){
  table_views[index,3] <- as.character(subset(movie_data,
                                              movie_data$movieId == table_views[index,1])$title)
}
table_views[1:6,]


ggplot(table_views[1:6, ], aes(x = title, y = views)) +
  geom_bar(stat="identity", fill = 'steelblue') +
  geom_text(aes(label=views), vjust=-0.3, size=3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Total Views of the Top Films")


movie_ratings <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                              colCounts(ratingMatrix) > 50]
movie_ratings





sampled_data<- sample(x = c(TRUE, FALSE),
                      size = nrow(movie_ratings),
                      replace = TRUE,
                      prob = c(0.8, 0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

recommen_model <- Recommender(data = training_data,
                              method = "IBCF",
                              parameter = list(k = 30))
recommen_model
class(recommen_model)



top_recommendations <- 10 # the number of items to recommend to each user
predicted_recommendations <- predict(object = recommen_model,
                                     newdata = testing_data,
                                     n = top_recommendations)
predicted_recommendations

user1 <- predicted_recommendations@items[[1]] # recommendation for the first user
movies_user1 <- predicted_recommendations@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10){
  movies_user2[index] <- as.character(subset(movie_data,
                                             movie_data$movieId == movies_user1[index])$title)
}
movies_user2


recommendation_matrix <- sapply(predicted_recommendations@items,
                                function(x){ as.integer(colnames(movie_ratings)[x]) }) # matrix with the recommendations for each user
#dim(recc_matrix)
recommendation_matrix[,1:10]

data("MovieLense")





summary(getRatings(MovieLense))

data.frame(ratings = getRatings(MovieLense)) %>%
  ggplot(aes(ratings)) + geom_bar(width = 0.75) +
  labs(title = 'Movielense Ratings Distribution')

train_proportion <- .75
# shouldn't keep n rec. items > min(rowCounts(movie_r))
min(rowCounts(MovieLense))

items_per_test_user_keep <- 10
# What's a good rating for a binary split?
good_threshold <- 4


# Building a Recommender System with R by Gorakala and Usuelli. Ch.4 pp 77 - 83
set.seed(123)
model_train_scheme <- MovieLense %>%
  evaluationScheme(method = 'split', # single train/test split
                   train = train_proportion, # proportion of rows to train.
                   given = items_per_test_user_keep, # shouldn't keep n rec. items > min(rowCounts(movie_r))
                   goodRating = good_threshold, # for binary classifier analysis.
                   k = 1)

# Building a Recommender System with R by Gorakala and Usuelli. Ch.4 pp 84
model_params <- list(method = "cosine",
                     nn = 10, # find each user's 10 most similar users.
                     sample = FALSE, # already did this.
                     normalize = "center")

model1 <- getData(model_train_scheme, "train") %>% #only fit on the 75% training data.
  Recommender(method = "UBCF", parameter = model_params)

model1_pred <- predict(model1, getData(model_train_scheme, "known"), type = "ratings")
model1_pred

test_error <- calcPredictionAccuracy(model1_pred, getData(model_train_scheme, "unknown"), byUser = TRUE)
head(test_error)
