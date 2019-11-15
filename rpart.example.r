# rpart is a popular implementation of the decision tree classifier
library(rpart)
library(rpart.plot)


# lets load in the fruit data from earlier
fruit.data <- data.frame(
  Width =  c(7.1, 7.9, 7.4, 8.2, 7.6, 7.8, 7, 7.1, 6.8, 6.6, 7.3, 7.2),
  Height = c(7.3, 7.5, 7, 7.3, 6.9, 8, 7.5, 7.9, 8, 7.7, 8.2, 7.9),
  Fruit = rep(c("Apple", "Pear"), each = 6, len = 12)
)


# building the decision tree via the rpart() function
fit <- rpart(formula = Fruit ~ Width + Height, method = "class", 
             data = fruit.data, minsplit = 1)
# formula: tells R that Fruit is the response variable being predicted using the features Width and Height
# method: "class" for classification tree, "anova" for regression tree
# data: input the dataframe
# minsplit: minimum number of observations per leaf, default is 20
summary(fit) # provides detailed info on each node, variable importance, ...
rpart.plot(fit) # exactly as described earlier!


# now lets use this tree to make some predictions
predict(fit) # the tree classifies the training data perfectly (as we would expect)
# lets see how it does on some brand new data
new.data <- data.frame(Width = c(8, 6.9), Height = c(7.2, 8.1))
new.data # one is short and fat, the other is slim and tall
predict(fit, new.data, type = 'class') # predicted as Apple and Pear as we would hope


# now lets se how this tree might be pruned
pruned.tree <- prune(fit, cp = 0.5) # this function uses cost-complexity pruning
rpart.plot(pruned.tree) # new tree has pruned off the second split


# and this is how we might have included an early stopping rule
# rpart.control() is the function in which we include our early stopping rules
?rpart.control # for all of the possible options
control <- rpart.control(minbucket = 2) # we define the minimum number of observations in a terminal node
# now lets fit a new decision tree with an early stopping rule via control
early.stopping.fit <- rpart(Fruit ~ Width + Height, method = "class", 
                            data = fruit.data, minsplit = 2, control = control)
rpart.plot(early.stopping.fit) # it worked, a less deep tree was created with one terminal node being impure

