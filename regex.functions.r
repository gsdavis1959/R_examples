library(devtools)
devtools::install_github("VerbalExpressions/RVerbalExpressions")
library(RVerbalExpressions)

strings = c('123Problem233','233Solution434','223Tim Apple444')

# Extract Strings
expr =  rx_alpha()
stringr::str_extract_all(strings,expr)

# Extract numbers
expr =  rx_digit()
stringr::str_extract_all(strings,expr)

# extract the name as a word
expr =  rx_alpha()  %>%  rx_word() %>% rx_alpha()
stringr::str_extract_all(strings,expr)

expr
