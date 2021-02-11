library(caTools)
library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)

head(iris)

set.seed(42)
sample_split <- sample.split(Y = iris$Species, SplitRatio = 0.75)
train_set <- subset(x = iris, sample_split == TRUE)
test_set <- subset(x = iris, sample_split == FALSE)

model <- rpart(Species ~ ., data = train_set, method = "class")
model

rpart.plot(model)

importances <- varImp(model)
importances %>%
  arrange(desc(Overall))

preds <- predict(model, newdata = test_set, type = "class")
preds

confusionMatrix(test_set$Species, preds)

