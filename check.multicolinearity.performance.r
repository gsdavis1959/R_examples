library(performance)
# fit model
data(mtcars)
model <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
# now check for multicollinearity
check_collinearity(model)

# plot results
x <- check_collinearity(model)
plot(x)


library(glmmTMB)
data(Salamanders)

# create highly correlated pseudo-variable
set.seed(1)
Salamanders$cover2 <-
  Salamanders$cover * runif(n = nrow(Salamanders), min = .7, max = 1.3)

# fit mixed model with zero-inflation
model <- glmmTMB(
  count ~ spp + mined + cover + cover2 + (1 | site), 
  ziformula = ~ spp + mined, 
  family = truncated_poisson, 
  data = Salamanders
)

# now check for multicollinearity
check_collinearity(model)
plot(check_collinearity(model))
