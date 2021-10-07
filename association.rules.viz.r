install.packages("arulesViz")
library(arulesViz)
library(tidyverse)

data("Groceries")
glimpse(Groceries)
head(Groceries)

rules <- apriori(Groceries, parameter = list(support = 0.005, confidence = 0.5))

plot(rules)

plot(rules, method = "graph", limit = 20)
