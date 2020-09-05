library(tidyverse)
data(mtcars)

mtcars <- mtcars %>% select(- c(vs, am))

pca <- prcomp(mtcars, center = TRUE,scale. = TRUE)

summary(pca)
# variance of each component to get the eignvalues. Remove any variable 
# with variance less than 1
pca$sdev ^ 2

# rotation
print(pca$rotation)


pca %>% biplot(cex = .5)


fit_1 <- lm(mpg ~ ., data = mtcars)

components <- cbind(mpg = mtcars[, "mpg"], pca$x[, 1:2]) %>%
  as.data.frame()

components

fit_2 <- lm(mpg ~ ., data = components)

summary(fit_1)$adj.r.squared
summary(fit_2)$adj.r.squared
