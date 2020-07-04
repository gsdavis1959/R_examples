library(recommenderlab)
library(tidyverse)
library(scales)
library(jsonlite)
library(knitr)
library(kableExtra)
library(ggrepel)
library(gridExtra)
library(lubridate)
library(tidytext)
library(wordcloud)
library(reshape2)
# Loading to pre-computed affinity data	



movies<-read.csv("movies.csv")
head(movies)
ratings<-read.csv('ratings.csv')
head(ratings)
affinity.data <- left_join(movies, ratings, by = 'movieId')
head(affinity.data)

affinity.matrix<- as(affinity.data,"realRatingMatrix")
as(affinity.matrix, 'list')


# Creation of the model - U(ser) B(ased) C(ollaborative) F(iltering)
Rec.model<-Recommender(affinity.matrix[1:5000], method = "UBCF")

# recommended top 5 items for user u15348
recommended.items <- predict(Rec.model, affinity.matrix[1126], n=5)
# to display them
as(recommended.items, "list")
# to obtain the top 3
recommended.items.top3 <- bestN(recommended.items, n = 3)
# to display them
as(recommended.items.top3, "list")


# Predict list of product which can be recommended to given users	 	
#to predict affinity to all non-rated items 
predicted.affinity <- predict(Rec.model, affinity.matrix["1",], type="ratings")
# to see the user "u15348"'s predicted affinity for items we didn't have any value for
df <- as.data.frame(as(predicted.affinity, "list"))
head(df, 50)
# .. and the real affinity for the items obtained from the affinity.matrix
as(affinity.matrix["1",], "list")


# create evaluation scheme splitting taking 90% of the date for training and leaving 10% for validation or test
e <- evaluationScheme(affinity.matrix[1:1000], method="split", train=0.8, given = 1)
# creation of recommender model based on ubcf
Rec.ubcf <- Recommender(getData(e, "train"), "UBCF")

# making predictions on the test data set
p.ubcf <- predict(Rec.ubcf, getData(e, "known"), type="ratings")

# obtaining the error metrics for both approaches and comparing them
error.ubcf<-calcPredictionAccuracy(p.ubcf, getData(e, "unknown"))

error.ubcf
p.ubcf

# user 3704

# second approach

tr<-read.csv("train_v2.csv",header=TRUE)
head(tr)
tr<-tr[,-c(1)]
# Check, if removed
tr[tr$user==1,]
head(tr)

g<-acast(tr, user ~ movie)
# Check the class of g
class(g)

# Convert it as a matrix
R<-as.matrix(g)

# Convert R into realRatingMatrix data structure
#   realRatingMatrix is a recommenderlab sparse-matrix like data-structure
r <- as(R, "realRatingMatrix")
r

# view r in other possible ways
as(r, "list")     # A list
as(r, "matrix")   # A sparse matrix

# I can turn it into data-frame
head(as(r, "data.frame"))

# normalize the rating matrix
r_m <- normalize(r)
r_m
as(r_m, "list")

# Draw an image plot of raw-ratings & normalized ratings
#  A column represents one specific movie and ratings by users
#   are shaded.
#   Note that some items are always rated 'black' by most users
#    while some items are not rated by many users
#     On the other hand a few users always give high ratings
#      as in some cases a series of black dots cut across items
image(r, main = "Raw Ratings")       
image(r_m, main = "Normalized Ratings")

# Can also turn the matrix into a 0-1 binary matrix
r_b <- binarize(r, minRating=1)
as(r_b, "matrix")

# Create a recommender object (model)
#   Run anyone of the following four code lines.
#     Do not run all four
#       They pertain to four different algorithms.
#        UBCF: User-based collaborative filtering
#        IBCF: Item-based collaborative filtering
#      Parameter 'method' decides similarity measure
#        Cosine or Jaccard
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Cosine",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="UBCF", param=list(normalize = "Z-score",method="Jaccard",nn=5, minRating=1))
rec=Recommender(r[1:nrow(r)],method="POPULAR")

# Depending upon your selection, examine what you got
print(rec)
names(getModel(rec))
getModel(rec)$nn

############Create predictions#############################
# This prediction does not predict movie ratings for test.
#   But it fills up the user 'X' item matrix so that
#    for any userid and movieid, I can find predicted rating
#     dim(r) shows there are 6040 users (rows)
#      'type' parameter decides whether you want ratings or top-n items
#         get top-10 recommendations for a user, as:
#             predict(rec, r[1:nrow(r)], type="topNList", n=10)
recom <- predict(rec, r[1:1000], type="ratings")
as(recom, "list")

########## Examination of model & experimentation  #############
########## This section can be skipped #########################

# Convert prediction into list, user-wise
as(recom, "list")
# Study and Compare the following:
as(r, "matrix")     # Has lots of NAs. 'r' is the original matrix
as(recom, "matrix") # Is full of ratings. NAs disappear
as(recom, "matrix")[,1:10] # Show ratings for all users for items 1 to 10
as(recom, "matrix")[5,3]   # Rating for user 5 for item at index 3
as.integer(as(recom, "matrix")[5,3]) # Just get the integer value
as.integer(round(as(recom, "matrix")[1000,8])) # Just get the correct integer value
as.integer(round(as(recom, "matrix")[368,1000])) 

# Convert all your recommendations to list structure
rec_list<-as(recom,"list")
head(summary(rec_list))
# Access this list. User 2, item at index 2
rec_list[[2]][2]
# Convert to data frame all recommendations for user 1
u1<-as.data.frame(rec_list[[1]])
attributes(u1)
class(u1)
# Create a column by name of id in data frame u1 and populate it with row names
u1$id<-row.names(u1)
# Check movie ratings are in column 1 of u1
u1
# Now access movie ratings in column 1 for u1
u1[u1$id==3952,1]


setwd('KaggleVersion')
links <- read.csv("links.csv")
movies <- read.csv("movies.csv",stringsAsFactors=FALSE)
ratings <- read.csv("ratings.csv")
tags <- read.csv("tags.csv")


## Data pre-processing
genres <- as.data.frame(movies$genres, stringsAsFactors=FALSE)
library(data.table)
genres2 <- as.data.frame(tstrsplit(genres[,1], '[|]', 
                                   type.convert=TRUE), 
                         stringsAsFactors=FALSE)
colnames(genres2) <- c(1:10)

genre_list <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western") # we have 18 genres in total

genre_matrix <- matrix(0,10330,18) #empty matrix, 10330=no of movies+1, 18=no of genres
genre_matrix[1,] <- genre_list #set first row to genre list
colnames(genre_matrix) <- genre_list #set column names to genre list

#iterate through matrix
for (i in 1:nrow(genres2)) {
  for (c in 1:ncol(genres2)) {
    genmat_col = which(genre_matrix[1,] == genres2[i,c])
    genre_matrix[i+1,genmat_col] <- 1
  }
}

#convert into dataframe
genre_matrix2 <- as.data.frame(genre_matrix[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (c in 1:ncol(genre_matrix2)) {
  genre_matrix2[,c] <- as.integer(genre_matrix2[,c])
} #convert from characters to integers

#Create a matrix to search for a movie by genre:
years <- as.data.frame(movies$title, stringsAsFactors=FALSE)
library(data.table)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
years <- as.data.frame(substr(substrRight(substrRight(years$`movies$title`, 6),5),1,4))

movies[,2]
substr(movies[,2],1,nchar(movies[,2])-6)
years
genre_matrix2 <- genre_matrix2[9742,]

search_matrix <- cbind(movies[,1], substr(movies[,2],1,nchar(movies[,2])-6), years, genre_matrix2)
colnames(search_matrix) <- c("movieId", "title", "year", genre_list)

write.csv(search_matrix, "search.csv")
search_matrix <- read.csv("search.csv", stringsAsFactors=FALSE)




## Assume that users like similar items, and retrieve movies 
# that are closest in similarity to a user's profile, which 
# represents a user's preference for an item's feature.
# use Jaccard Distance to measure the similarity between user profiles

# The User-Based Collaborative Filtering Approach

library(reshape2)
ratings <- ratings[1:500,]
#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

# Method: UBCF
# Similarity Calculation Method: Cosine Similarity
# Nearest Neighbors: 30

library(recommenderlab)
#Convert rating matrix into a recommenderlab sparse matrix
ratingmat <- as(ratingmat, "realRatingMatrix")

# Determine how similar the first four users are with each other
# create similarity matrix
similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
image(as.matrix(similarity_users), main = "User similarity")

# compute similarity between
# the first four movies
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
image(as.matrix(similarity_items), main = "Item similarity")

# Exploring values of ratings:
vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # what are unique values of ratings

table_ratings <- table(vector_ratings) # what is the count of each rating value
table_ratings

# Visualize the rating:
vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings")

# Exploring viewings of movies:
views_per_movie <- colCounts(ratingmat) # count views for each movie

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) # create dataframe of views
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] # sort by number of views

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels=subset(movies2, movies2$movieId == table_views$movie)$title) +
  ggtitle("Number of views of the top movies")

#Visualizing the matrix:
image(ratingmat, main = "Heatmap of the rating matrix") # hard to read-too many dimensions
image(ratingmat[1:10, 1:15], main = "Heatmap of the first rows and columns")
image(ratingmat[rowCounts(ratingmat) > quantile(rowCounts(ratingmat), 0.99),
                colCounts(ratingmat) > quantile(colCounts(ratingmat), 0.99)], 
      main = "Heatmap of the top users and movies")


#Normalize the data
ratingmat_norm <- normalize(ratingmat)
image(ratingmat_norm[rowCounts(ratingmat_norm) > quantile(rowCounts(ratingmat_norm), 0.99),
                     colCounts(ratingmat_norm) > quantile(colCounts(ratingmat_norm), 0.99)], 
      main = "Heatmap of the top users and movies")

#Create UBFC Recommender Model. UBCF stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

model_details <- getModel(recommender_model)
model_details$data

recom <- predict(recommender_model, 
                 ratingmat[1], 
                 n=10) #Obtain top 10 recommendations for 1st user in dataset

recom

recc_matrix <- sapply(recom@items, 
                      function(x){ colnames(ratingmat)[x] })
dim(recc_matrix)

recom_list <- as(recom, 
                 "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}


# Evaluation:
evaluation_scheme <- evaluationScheme(ratingmat, 
                                      method="cross-validation", 
                                      k=5, given=3, 
                                      goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, 
                               method="UBCF", 
                               n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]
eval_results




movies <- read_csv('tmdb_5000_movies.csv', na="NA")
credits <- read_csv("tmdb_5000_credits.csv",  na="NA")

glimpse(movies)

#The trick on how to do this is copied from kernel: https://www.kaggle.com/epfreed/tidydata-movie-dataset-exploration/report

keywords <- movies %>%    
  filter(nchar(keywords)>2) %>%         # fiter out blank keywords field
  mutate(                               # create a new field 
    js = lapply(keywords, fromJSON)     #   containing a LIST of keyword and value pairs
  ) %>%                                 #   called id and name
  unnest(js, .name_repair = "unique") %>%                        # turn each keyword/value pairs in the LIST into a row
  select(id, title, keyword=name) %>%   # select the columns we want
  mutate_if(is.character, factor)

genres <- movies %>%
  filter(nchar(genres)>2) %>%
  mutate(js = lapply(genres, fromJSON)) %>%
  unnest(js, .name_repair = "unique") %>%
  select(id, title, genres=name) %>%
  mutate_if(is.character, factor)

genres[1:14,]

genres3 <- genres
genres3$order <- 0
genres3$order[1] <- 1


for(i in 1:(nrow(genres3)-1)) {
  if(genres3$id[i+1]!=genres3$id[i]){
    genres3$order[i+1] <- 1
  } else {genres3$order[i+1] <- (genres3$order[i])+1}
}

genres3 <- genres3 %>% filter(order < 4) %>%
  spread(key=order, value=genres) %>%
  rename(genre_1="1", genre_2="2", genre_3="3")

movies <- left_join(movies, genres3 %>% select(id, genre_1, genre_2, genre_3), by="id")

genres3 %>% filter(id %in% c("19995", "285", "206647", "49026"))

glimpse(credits)

cast <- credits %>%
  filter(nchar(cast)>2) %>%
  mutate(js = lapply(cast, fromJSON)) %>%
  unnest(js) %>%
  select(-cast, -crew, -credit_id) %>%
  rename(actor=name, movie_cast_id=cast_id, actor_id=id) %>%
  mutate_if(is.character, factor)

glimpse(cast)

cast %>% filter(actor=="Leonardo DiCaprio")

cast %>% filter(title=="Titanic") %>% select(title, order, actor, character) %>% arrange(order) %>% slice(1:10)

cast1 <- cast %>% count(actor) # for visualisation later on

cast <- cast %>% filter(order %in% c(0, 1, 2)) %>% select(movie_id, title, order, actor)
cast %>% filter(movie_id==453)

cast$order[1] <- 0
for (i in 1:(nrow(cast)-1)){
  if(cast$movie_id[i+1]!=cast$movie_id[i]){
    cast$order[i+1] <- 0
  } else {cast$order[i+1] <- cast$order[i]+1}
}

cast <- cast %>% filter(order %in% c(0, 1, 2)) %>%
  #cast <- cast %>% spread(key=order, value=actor)
  spread(key=order, value=actor) %>%
  rename(actor_1="0", actor_2="1", actor_3="2")

cast %>% filter(title %in% c("Titanic", "Avatar", "A Beautiful Mind"))

movies <- left_join(movies, cast %>% select(id=movie_id, actor_1, actor_2, actor_3), by = "id")

crew <- credits %>%
  filter(nchar(crew)>2) %>%
  mutate(js = lapply(crew, fromJSON)) %>%
  unnest(js) %>%
  select(-cast, -crew, -credit_id) %>%
  rename(crew=name, crew_id=id) %>%
  mutate_if(is.character, factor)

glimpse(crew)

crew %>% filter(crew=="James Cameron")

movies1Director <- crew %>% filter(job=="Director") %>% count(movie_id) %>% filter(n==1)

movies <- left_join(movies, crew %>% filter(job=="Director" & movie_id %in% movies1Director$movie_id) %>% select(id=movie_id, director=crew), by = "id")

range(movies$vote_count)

movies %>%
  ggplot(aes(x=vote_count)) +
  geom_histogram(fill="blue", binwidth = 50) +
  scale_x_continuous(breaks=seq(0, 5000, by=500), label=comma) +
  coord_cartesian(x=c(0, 5000)) +
  labs(x="number of votes", y="number of movies")

movies  %>% top_n(20, wt=vote_count) %>%
  ggplot(aes(x=reorder(title, vote_count), y=vote_count)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0, 15000)) +
  labs(x="", y="Number of votes") +
  geom_text(aes(label=vote_count), hjust=-0.1, size=3) +
  geom_text(aes(label=vote_average), y=1000, size=3, col="yellow")

movies %>%
  ggplot(aes(x=vote_average)) +
  geom_histogram(fill="blue", binwidth = 0.1) +
  scale_x_continuous(breaks=seq(0, 10, by=1)) +
  labs(x="vote average", y="number of movies")

psych::skew(movies$vote_average)

movies %>% filter((vote_average<4.5|vote_average>8.5)& vote_count>=400) %>%
  select(id, title, vote_average, vote_count) %>% arrange(vote_average)

movies %>% filter(vote_count > 250) %>% top_n(20, wt=vote_average) %>%
  ggplot(aes(x=reorder(title, vote_average), y=vote_average)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0,10)) +
  labs(x="", y="Voting Average") +
  geom_text(aes(label=vote_average), hjust=-0.1, size=3) +
  scale_y_continuous(breaks=seq(0, 10, by=1)) +
  geom_text(aes(label=vote_count), y=0.4, size=3, col="yellow")

genres %>% group_by(genres) %>% count() %>%
  ggplot(aes(x=reorder(genres, n), y=n)) +
  geom_col(fill="blue") + coord_flip() +
  labs(x="", y="Number of movies")

genres250 <- left_join(genres, movies %>% filter(vote_count > 250) %>% select(id, vote_average, vote_count, original_language, original_title), by = "id")
genres250 <- genres250 %>% filter(!is.na(genres250$vote_average))
genres250$title <- str_trunc(as.character(genres250$title), width = 50, side="right")
genres250$title <- as.factor(genres250$title)

genres250 %>% filter(!genres %in% c("Foreign", "TV Movie")) %>%
  group_by(genres) %>% arrange(desc(vote_average)) %>% slice(1:10) %>%
  ggplot(aes(x=reorder(title, vote_average), y=vote_average)) +
  geom_col(aes(fill=genres), show.legend = FALSE) + coord_flip(y=c(6,8.5)) +
  facet_wrap(~genres, scales = "free_y", ncol=2) +
  labs(x="", y="") +
  theme(axis.text.y = element_text(size=6))

cast1 %>% top_n(20, wt=n)%>%
  ggplot(aes(x=reorder(actor, n), y=n)) +
  geom_col(fill="blue") + coord_flip() +
  labs(x="", y="Number of movies")

movies %>% filter(!is.na(movies$director)) %>% count(director) %>% top_n(20, wt=n)%>%
  ggplot(aes(x=reorder(director, n), y=n)) +
  geom_col(fill="blue") + coord_flip() +
  labs(x="", y="Number of movies")

n_distinct(keywords$keyword)

keywords %>% count(keyword) %>% top_n(20, wt=n) %>%
  ggplot(aes(x=reorder(keyword, n), y=n)) +
  geom_col(fill="blue") + coord_flip() +
  labs(x="", y="Number of movies")

set.seed(2019)

keywords_counts <- keywords %>% count(keyword)

par(mfrow=c(1, 1),bg="grey97")
wordcloud(keywords_counts$keyword, keywords_counts$n, max.words = 100, scale=c(2.0,.5), random.color = TRUE, random.order=FALSE, rot.per=0, colors=brewer.pal(9,"Set1"))

dummy <- keywords %>% count(id, title)
dummy %>%
  ggplot() +
  geom_histogram(aes(x=n), breaks = seq(0, 100, by=2), fill="blue") +
  scale_x_continuous(breaks = seq(0, 100, by=10)) +
  labs(x="Number of keywords per movie. Binwidth=2", y= "Number of Movies")

dummy <- keywords %>% count(id) %>% filter(n<3)
nrow(dummy)

dummy <- anti_join(movies, keywords, by="id")
nrow(dummy)

movies$original_language <- as.factor(movies$original_language)
plotly::ggplotly(movies %>% group_by(original_language) %>% count() %>% ungroup() %>% top_n(10, wt=n) %>%
                   ggplot(aes(x=reorder(original_language, n), y=n)) +
                   geom_bar(stat="identity", fill="blue") + coord_flip() +
                   labs(x="", y="Number of movies"), tooltip = "y")

movies$release_year <- year(movies$release_date)

plotly::ggplotly(movies %>%
                   ggplot(aes(x=release_year)) +
                   geom_histogram(fill="blue", binwidth = 1) +
                   labs(x="Release year", y="Number of movies"))

range(movies$release_date[movies$release_year==2016], na.rm=TRUE)

C <- mean(movies$vote_average)
m <- quantile(movies$vote_count, 0.75)

movies$weighted_rating <- (movies$vote_average*movies$vote_count + C*m)/(movies$vote_count + m)

movies %>% top_n(20, wt=weighted_rating) %>%
  ggplot(aes(x=reorder(title, weighted_rating), y=weighted_rating)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0,10)) +
  labs(x="", y="Weighted Rating") +
  geom_text(aes(label=round(weighted_rating, 2)), hjust=-0.1, size=3) +
  scale_y_continuous(breaks=seq(0, 10, by=1)) +
  geom_text(aes(label=paste("Votes:", vote_count, "Vote Average:", vote_average)), y=2.3, size=3, col="yellow")

genres250 <- left_join(genres250, movies %>% select(id, weighted_rating), by="id")

recommendGenreLanguage <- function(Genre, Language="en") {
  genres250 %>% filter(original_language==Language & genres==Genre) %>%
    arrange(desc(weighted_rating)) %>% slice(1:5)
}

recommendGenreLanguage("Drama")
recommendGenreLanguage("Drama", "es")

similarity_vars <- c("actor_1", "actor_2", "actor_3", "director", "genre_1", "genre_2", "genre_3")

movies_filter <- movies %>% select(id, title, !!similarity_vars)
movies_filter <- movies_filter %>% mutate_if(is.factor, as.character)

recommend_similar <- function(movie){
  director <- movies_filter$director[movies_filter$id==movie]
  actor1 <- movies_filter$actor_1[movies_filter$id==movie]
  actor2 <- movies_filter$actor_2[movies_filter$id==movie]
  actor3 <- movies_filter$actor_3[movies_filter$id==movie]
  genre1 <- movies_filter$genre_1[movies_filter$id==movie]
  genre2 <- movies_filter$genre_2[movies_filter$id==movie]
  genre3 <- movies_filter$genre_3[movies_filter$id==movie]
  
  rec_df <- movies_filter
  
  rec_df$same_director <- NA
  rec_df$same_a1 <- NA
  rec_df$same_a2 <- NA
  rec_df$same_a3 <- NA
  rec_df$same_g1 <- NA
  rec_df$same_g2 <- NA
  rec_df$same_g3 <- NA
  
  rec_df$same_director <- ifelse(rec_df$director==director, 1, 0)
  rec_df$same_a1 <- ifelse(rec_df$actor_1==actor1|rec_df$actor_2==actor1|rec_df$actor_3==actor1, 1, 0)
  rec_df$same_a2 <- ifelse(rec_df$actor_1==actor2|rec_df$actor_2==actor2|rec_df$actor_3==actor2, 1, 0)
  rec_df$same_a3 <- ifelse(rec_df$actor_1==actor3|rec_df$actor_2==actor3|rec_df$actor_3==actor3, 1, 0)
  rec_df$same_g1 <- ifelse(rec_df$genre_1==genre1|rec_df$genre_2==genre1|rec_df$genre_3==genre1, 1, 0)
  rec_df$same_g2 <- ifelse(rec_df$genre_1==genre2|rec_df$genre_2==genre2|rec_df$genre_3==genre2, 1, 0)
  rec_df$same_g3 <- ifelse(rec_df$genre_1==genre3|rec_df$genre_2==genre3|rec_df$genre_3==genre3, 1, 0)
  
  rec_df <- rec_df %>% mutate_at(vars("same_director": "same_g3"), list(~replace(., is.na(.), 0)))
  
  rec_df$sim_count <- rowSums(rec_df[,10:16])
  
  rec_df <- left_join(rec_df, movies %>% select(id, weighted_rating), by="id")
  
  Top5_rec <- rec_df %>% arrange(desc(sim_count), desc(weighted_rating)) %>% slice(1:6) %>% select(id, title, sim_count, weighted_rating, everything())
  
  kable(Top5_rec) %>%
    kable_styling(full_width=TRUE)
}

recommend_similar(597)
recommend_similar(680)

recommend_similar2 <- function(movie){
  director <- movies_filter$director[movies_filter$id==movie]
  actor1 <- movies_filter$actor_1[movies_filter$id==movie]
  actor2 <- movies_filter$actor_2[movies_filter$id==movie]
  actor3 <- movies_filter$actor_3[movies_filter$id==movie]
  genre1 <- movies_filter$genre_1[movies_filter$id==movie]
  genre2 <- movies_filter$genre_2[movies_filter$id==movie]
  genre3 <- movies_filter$genre_3[movies_filter$id==movie]
  
  rec_df <- movies_filter
  
  rec_df$same_director <- NA
  rec_df$same_a1 <- NA
  rec_df$same_a2 <- NA
  rec_df$same_a3 <- NA
  rec_df$same_g1 <- NA
  rec_df$same_g2 <- NA
  rec_df$same_g3 <- NA
  
  rec_df$same_director <- ifelse(rec_df$director==director, 2, 0)
  rec_df$same_a1 <- ifelse(rec_df$actor_1==actor1|rec_df$actor_2==actor1|rec_df$actor_3==actor1, 1, 0)
  rec_df$same_a2 <- ifelse(rec_df$actor_1==actor2|rec_df$actor_2==actor2|rec_df$actor_3==actor2, 1, 0)
  rec_df$same_a3 <- ifelse(rec_df$actor_1==actor3|rec_df$actor_2==actor3|rec_df$actor_3==actor3, 1, 0)
  rec_df$same_g1 <- ifelse(rec_df$genre_1==genre1|rec_df$genre_2==genre1|rec_df$genre_3==genre1, 1, 0)
  rec_df$same_g2 <- ifelse(rec_df$genre_1==genre2|rec_df$genre_2==genre2|rec_df$genre_3==genre2, 1, 0)
  rec_df$same_g3 <- ifelse(rec_df$genre_1==genre3|rec_df$genre_2==genre3|rec_df$genre_3==genre3, 1, 0)
  
  rec_df <- rec_df %>% mutate_at(vars("same_director": "same_g3"), list(~replace(., is.na(.), 0)))
  
  rec_df$sim_count <- rowSums(rec_df[,10:16])
  
  rec_df <- left_join(rec_df, movies %>% select(id, weighted_rating), by="id")
  
  Top5_rec <- rec_df %>% arrange(desc(sim_count), desc(weighted_rating)) %>% slice(1:6) %>% select(id, title, sim_count, weighted_rating, everything())
  
  kable(Top5_rec) %>%
    kable_styling(full_width=TRUE)
}

recommend_similar2(597)

# text based
plots <- movies %>% select(id, title, text=overview)

plots$text[plots$id==597]

plots_words <- plots %>%
  unnest_tokens(word, text)  %>%
  count(id, title, word, sort=TRUE)

plots_words <- plots_words %>% filter(!str_detect(word, "[0-9]")) #removing words containing numbers

total_words <- plots_words %>% group_by(id, title) %>% summarize(total = sum(n))
plots_words <- left_join(plots_words, total_words, by = c("id", "title"))

plots_words <- plots_words %>% bind_tf_idf(word, id, n)
plots_words <- plots_words %>% filter(idf<=log(length(unique(plots_words$id))/10)) #only words that are in at least 10 plots

slice <- plots_words %>% group_by(id) %>% arrange(desc(tf_idf)) %>% slice(1:10)
kable(slice %>% filter(id==597)) %>%
  kable_styling(full_width=TRUE)
keywords %>% filter(id==597)


key_count <- keywords %>% group_by(title, keyword) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% print()


dtm <- key_count %>% cast_dtm(title, keyword, count)
library(quanteda)
m <- key_count %>%
  cast_dfm(title, keyword, count) %>% print()

m

m2 <- key_count %>%
  cast_sparse(title, keyword, count)
class(m2)
dim(m2)

movies_matrix <- as(m2, "realRatingMatrix")
class(movies_matrix)

# new dataset
ratings_small <- read_csv("ratings_small.csv", na="NA", col_types = 
                            cols(
                              userId = col_character(),
                              movieId = col_character(),
                              rating = col_double(),
                              timestamp = col_double()
                            )
)

ratings_small$timestamp <- as.POSIXct(ratings_small$timestamp, tz="UTC", origin='1970-01-01')

ratings_small

rat_mat <- ratings_small %>% select(-timestamp) %>% 
  spread(movieId, rating) %>%
  remove_rownames %>%
  column_to_rownames(var="userId")

dim(rat_mat)

rat_mat <- as.matrix(rat_mat)
dimnames(rat_mat) <- list(user= rownames(rat_mat), item = colnames(rat_mat))


movies_matrix <- as(rat_mat, "realRatingMatrix")
class(movies_matrix)

