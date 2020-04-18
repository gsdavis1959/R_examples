library(caret)
library(tidyverse)

data(mtcars)    # Load the dataset
head(mtcars)
?mtcars

# Simple linear regression model (lm means linear model)
model1 <- train(mpg ~ wt,
               data = mtcars,
               method = "lm")

# Multiple linear regression model
model2 <- train(mpg ~ .,
               data = mtcars,
               method = "lm")

# Ridge regression model
model3 <- train(mpg ~ .,
               data = mtcars,
               method = "ridge") # Try using "lasso"

fitControl <- trainControl(method = "repeatedcv",   
                           number = 10,     # number of folds
                           repeats = 10)    # repeated ten times

model.cv <- train(mpg ~ .,
                  data = mtcars,
                  method = "lasso",  # now we're using the lasso method
                  trControl = fitControl)  

model.cv 

# with preprocessing
model.cv <- train(mpg ~ .,
                  data = mtcars,
                  method = "lasso",
                  trControl = fitControl,
                  preProcess = c('scale', 'center')) # default: no pre-processing

?train    # if you need more information about the train function
model.cv

# Here I generate a dataframe with a column named lambda with 100 values that goes from 10^10 to 10^-2
lambdaGrid <- expand.grid(lambda = 10^seq(10, -2, length=100))

model.cv <- train(mpg ~ .,
                  data = mtcars,
                  method = "ridge",
                  trControl = fitControl,
                  preProcess = c('scale', 'center'),
                  tuneGrid = lambdaGrid,   # Test all the lambda values in the lambdaGrid dataframe
                  na.action = na.omit)   # Ignore NA values

model.cv
library(plotly)
ggplotly(ggplot(varImp(model.cv)))

predictions <- predict(model.cv, mtcars)

predictions
