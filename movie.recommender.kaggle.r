#loading the libraries needed
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
library(recommenderlab)

movies <- read_csv('KaggleVersion/tmdb_5000_movies.csv', na="NA")
credits <- read_csv("KaggleVersion/tmdb_5000_credits.csv",  na="NA")

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
ratings_small <- read_csv("KaggleVersion/ratings_small.csv", na="NA", col_types = 
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

