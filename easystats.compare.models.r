library(performance)


model1 <- lm(Sepal.Length ~ Petal.Length, data = iris)
model2 <- lm(Sepal.Length ~ Petal.Width, data = iris)
model3 <- lm(Sepal.Length ~ Sepal.Width, data = iris)




library(insight)
library(magrittr) # for pipe operator

# we will use `print_md` function to display a well-formatted table
performance(model1) %>%
  print_md()


compare_performance(model1, model2, model3) %>%
  print_md()


library(see)

plot(compare_performance(model1, model2, model3))


test_performance(model1, model2, model3) %>%
  print_md()


if (FALSE) {
  m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
  check_model(m)
  
  if (require("lme4")) {
    m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
    check_model(m, panel = FALSE)
  }
  
  if (require("rstanarm")) {
    m <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
    check_model(m)
  }
}

