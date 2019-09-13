library(stringr)
library(text2vec)
data("movie_review")
# select 500 rows for faster running times
movie_review = movie_review[1:500, ]
prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}
movie_review$review_clean = prep_fun(movie_review$review)

doc_set_1 = movie_review[1:300, ]
it1 = itoken(doc_set_1$review_clean, progressbar = FALSE)

# specially take different number of docs in second set
doc_set_2 = movie_review[301:500, ]
it2 = itoken(doc_set_2$review_clean, progressbar = FALSE)


it = itoken(movie_review$review_clean, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)
# Jaccard similarity
# proportion of number of common words to number of unique words in both documets

# they will be in the same space because we use same vectorizer
# hash_vectorizer will also work fine
dtm1 = create_dtm(it1, vectorizer)
dim(dtm1)

dtm2 = create_dtm(it2, vectorizer)
dim(dtm2)
d1_d2_jac_sim = sim2(dtm1, dtm2, method = "jaccard", norm = "none")
dim(d1_d2_jac_sim)
d1_d2_jac_sim[1:2, 1:5]

dtm1_2 = dtm1[1:200, ]
dtm2_2 = dtm2[1:200, ]
d1_d2_jac_psim = psim2(dtm1_2, dtm2_2, method = "jaccard", norm = "none")
str(d1_d2_jac_psim)

# Cosine similarity
d1_d2_cos_sim = sim2(dtm1, dtm2, method = "cosine", norm = "l2")
d1_d2_cos_sim[1:2, 1:5]

# Cosine similarity with Tf-Idf
dtm = create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)

d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
d1_d2_tfidf_cos_sim[1:2, 1:5]

# Cosine similarity with LSA
lsa = LSA$new(n_topics = 100)
dtm_tfidf_lsa = fit_transform(dtm_tfidf, lsa)
d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf_lsa, method = "cosine", norm = "l2")
d1_d2_tfidf_cos_sim[1:2, 1:5]

# parallel similarities
x = dtm_tfidf_lsa[1:250, ]
y = dtm_tfidf_lsa[251:500, ]
head(psim2(x = x, y = y, method = "cosine", norm = "l2"))

# Euclidean distance
x = dtm_tfidf_lsa[1:300, ]
y = dtm_tfidf_lsa[1:200, ]
m1 = dist2(x, y, method = "euclidean")
m2 = dist2(x, y, method = "euclidean", norm = "l1")
m3 = dist2(x, y, method = "euclidean", norm = "none")

# create topics
tokens = movie_review$review[1:4000] %>% 
  tolower %>% 
  word_tokenizer
it = itoken(tokens, ids = movie_review$id[1:4000], progressbar = FALSE)
v = create_vocabulary(it) %>% 
  prune_vocabulary(term_count_min = 10, doc_proportion_max = 0.2)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer, type = "dgTMatrix")

lda_model = LDA$new(n_topics = 10, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)
barplot(doc_topic_distr[1, ], xlab = "topic", 
        ylab = "proportion", ylim = c(0, 1), 
        names.arg = 1:ncol(doc_topic_distr))
lda_model$get_top_words(n = 10, topic_number = c(1L, 5L, 10L), lambda = 1)
lda_model$get_top_words(n = 10, topic_number = c(1L, 5L, 10L), lambda = 0.2)

# apply to new model
new_dtm = itoken(movie_review$review[4001:5000], tolower, word_tokenizer, ids = movie_review$id[4001:5000]) %>% 
  create_dtm(vectorizer, type = "dgTMatrix")
new_doc_topic_distr = lda_model$transform(new_dtm)
perplexity(new_dtm, topic_word_distribution = lda_model$topic_word_distribution, doc_topic_distribution = new_doc_topic_distr)
lda_model$plot()
