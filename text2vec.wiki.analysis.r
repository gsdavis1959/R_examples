library(text2vec)
text8_file = "~/temp/text8"
if (!file.exists(text8_file)) {
  download.file("http://mattmahoney.net/dc/text8.zip", "~/text8.zip")
  unzip ("~/text8.zip", files = "text8", exdir = "~/temp")
}
wiki = readLines(text8_file, n = 1, warn = FALSE)

# Create iterator over tokens
tokens = space_tokenizer(wiki)
# Create vocabulary. Terms will be unigrams (simple words).
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)

# words that appear more than 5 times
vocab = prune_vocabulary(vocab, term_count_min = 5L)

# Use our filtered vocabulary
vectorizer = vocab_vectorizer(vocab)
# use window of 5 for context words
tcm = create_tcm(it, vectorizer, skip_grams_window = 5L)

glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)

dim(wv_main)

# second model
glove = GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
# `glove` object will be modified by `fit_transform()` call !
wv_main = fit_transform(tcm, glove, n_iter = 20)

wv_context = glove$components
dim(wv_context)

word_vectors = wv_main + t(wv_context)

berlin = word_vectors["paris", , drop = FALSE] - 
  word_vectors["france", , drop = FALSE] + 
  word_vectors["germany", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = berlin, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)


queen = word_vectors["king", , drop = FALSE] - 
  word_vectors["man", , drop = FALSE] + 
  word_vectors["woman", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = queen, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)


guess = word_vectors["doctor", , drop = FALSE] - 
  word_vectors["man", , drop = FALSE] + 
  word_vectors["nurse", , drop = FALSE]
cos_sim = sim2(x = word_vectors, y = guess, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 5)


saveRDS(glove, 'glove_model.rds')
