# data wrangling
library(tidyverse)

# text processing
library(tidytext)
library(textclean)
library(tokenizers)

# markov chain
library(markovchain)

# read data
tlp <- read.delim("C:/Users/gsdav/Documents/Data/Datasets/the_little_prince.txt",col.names = "text")

head(tlp,10)

tlp_clean <- tlp %>% 
  slice(-1) %>% # remove first line (version info)
  filter(!str_detect(text, "[/:]"), # remove lines with certain characters
         !str_detect(text, "Chapter")) # remove lines with certain string
head(tlp_clean)

tlp_clean  <- tlp_clean %>% 
  mutate(text = tolower(text) %>% # tolower sentences
           replace_contraction() %>%  # expand contraction
           replace_white() %>%  # replace double white space into single space
           str_remove_all(pattern = "lpp_1943.") %>% # remove pattern
           str_remove_all(pattern = "[0-9]") %>% # remove numbers
           str_remove_all(pattern = "[()]") %>% # remove specific punctuation
           str_remove_all(pattern = "--") %>%
           str_replace_all(pattern = " - ", replacement = "-") %>%  # replace pattern
           str_replace_all(pattern = "n't", replacement = "not") %>% 
           str_remove(pattern = "[.]") %>% # remove first matched pattern
           str_remove(pattern = " "))


# glimpse data; first 10 sentences
head(tlp_clean, 10)


# split words from sentences
text_tlp <- tlp_clean %>% 
  pull(text) %>% 
  strsplit(" ") %>% 
  unlist() 

text_tlp %>% head(27)

fit_markov <- markovchainFit(text_tlp)
fit_markov

create_me <- function(num = 5, first_word = "i", n = 2) {
  
  
  for (i in 1:num) {
    
    set.seed(i+5)
    
    markovchainSequence(n = n, # generate 2 additional random words
                        markovchain = fit_markov$estimate,
                        t0 = tolower(first_word), include.t0 = T) %>% 
      # joint words
      paste(collapse = " ") %>% # join generated words with space
      # create proper sentence form
      str_replace_all(pattern = " ,", replacement = ",") %>% 
      str_replace_all(pattern = " [.]", replacement = ".") %>% 
      str_replace_all(pattern = " [!]", replacement = "!") %>% 
      str_to_sentence() %>% # start every sentences with capitalization
      print()
    
    
  }
  
}
# generate text
create_me(num = 5, first_word = "little", n = 10)

# random words
random_vocab <- function(n = 10, seed = NULL) {
  
  set.seed(seed)
  unique_vocab <- tlp_clean %>% 
    mutate(text = text %>% 
             str_remove_all("[:punct:]")) %>% 
    pull(text) %>% 
    strsplit(" ") %>% 
    unlist() %>% 
    unique()
  
  unique_vocab[sample(length(unique_vocab), n)]
  
}

random_vocab(n = 10, seed = 123)


# another approach

library(stringr)

# load in the data
texts <- read.csv("C:/Users/gsdav/Documents/Data/Datasets/shortjokes.csv", stringsAsFactors = FALSE)

head(texts)

# get rid of empy titles
text <- tlp_clean[nchar(tlp_clean) > 0]

# remove all punctuation marks
text <- str_replace_all(text, "[[:punct:]]", "")

# get a list of just tokens
terms <- unlist(strsplit(text, ' '))

# check out the first few
head(terms)

# create our model (this bit will take a minute)
fit <- markovchainFit(data = terms)

# save out some new titles
titles <- NULL

# generate 1000 new titles
for(i in 1:100){
  titles <- c(titles, 
              c(paste(markovchainSequence(n=4, markovchain=fit$estimate), collapse=' ')),
              "/n") 
}

# Check out the first few
titles
