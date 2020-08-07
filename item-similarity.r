library(lsa)
library(tidyverse)
ratings = read.csv('ratings.csv') %>% print()
sub_ratings <- ratings %>% select(userId, movieId, rating)
sub_ratings_wide <- sub_ratings %>% pivot_wider(names_from = movieId, values_from = rating) %>% print()

sub_ratings_wide <- sub_ratings_wide %>% select(1:100) %>% print()

x = sub_ratings_wide[,2:100]
x[is.na(x)] = 0
item_sim = cosine(as.matrix(x))
item_sim

rec_itm_for_user = function(userno)
{
  #extract all the movies not rated by CHAN
  userRatings = sub_ratings_wide[userno,]
  non_rated_movies = list()
  rated_movies = list()
  for(i in 2:ncol(userRatings)){
    if(is.na(userRatings[,i]))
    {
      non_rated_movies = c(non_rated_movies,colnames(userRatings)[i])
    }
    else
    {
      rated_movies = c(rated_movies,colnames(userRatings)[i])
    }
  }
  non_rated_movies = unlist(non_rated_movies)
  rated_movies = unlist(rated_movies)
  #create weighted similarity for all the rated movies by CHAN
  non_rated_pred_score = list()
  for(j in 1:length(non_rated_movies)){
    temp_sum = 0
    df = item_sim[which(rownames(item_sim)==non_rated_movies[j]),]
    for(i in 1:length(rated_movies)){
      temp_sum = temp_sum+ df[which(names(df)==rated_movies[i])]
    }
    weight_mat = df*sub_ratings_wide[userno,2:100]
    non_rated_pred_score = c(non_rated_pred_score,rowSums(weight_mat,na.rm=T)/temp_sum)
  }
  pred_rat_mat = as.data.frame(non_rated_pred_score)
  names(pred_rat_mat) = non_rated_movies
  for(k in 1:ncol(pred_rat_mat)){
    sub_ratings_wide[userno,][which(names(sub_ratings_wide[userno,]) == names(pred_rat_mat)[k])] = pred_rat_mat[1,k]
  }
  return(sub_ratings_wide[userno,])
}

rec_itm_for_user(80)
