# install.packages('devtools')
devtools::install_github('https://github.com/paulvanderlaken/ppsr')
library(ppsr)

# predictive power score

score(x = iris$Sepal.Length, y = iris$Sepal.Width)

score_predictors(df = iris, y = 'Species')

score_matrix(df = iris)

visualize_predictors(df = iris, y = 'Species')

visualize_matrix(df = iris)
