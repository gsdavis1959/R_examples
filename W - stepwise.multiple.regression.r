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

# alternative method

library(olsrr)
model <- (Fertility ~.)
fit <- lm(model, swiss)
test <- ols_step_all_possible(fit)
plot(test)


ols_step_all_possible(fit, pent = 0.1, prem = 0.3, details = TRUE)
