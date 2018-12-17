library(tidyverse)
library(caret)
library(leaps)
library(MASS)
df <- as.matrix(swiss)
cor(df)
# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)
summary(full.model)
plot(full.model)
# Stepwise regression model
step.model <- step(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)
