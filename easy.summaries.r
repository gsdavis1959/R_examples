library(purrr)
library(tidyverse)
data(mtcars)
mtcars %>% split(mtcars$carb) %>% map(summary)

library(skimr)
skim(mtcars)

library(dplyr)

group_by(mtcars, cyl) %>% skim()
data("iris")
data <- as.data.frame(iris)

library(tableone)
CreateTableOne(data = data)
summary(CreateTableOne(data = data))

CreateTableOne(strata = "Species", data = data)
print(CreateTableOne(strata = "Species", data = data), nonnormal = "score")


library(desctable)
desctable(data)

group_by(data, Species) %>%
desctable()
desctable(data,stats = list("N" = length, "Mean" = mean, "SD" = sd, "Min" = min, "Max" = max))

library(GGally)
ggpairs(data)
ggpairs(data, mapping = aes(colour = Species))

library(descriptr)
ds_summary_stats(data)
ds_screener(data)
ds_group_summary(data$category, data$rating)
ds_freq_table(data$Species)

mean.cars = with(mtcars, sapply(mtcars[c('mpg', 'disp',  'hp')], mean))
sd.cars = with(mtcars, sapply(mtcars[c('mpg', 'disp',  'hp')], sd)); sd.cars
n.cars = with(mtcars, sapply(mtcars[c('mpg', 'disp',  'hp')], length)); n.cars
cbind(n.cars, mean.cars, sd.cars)
round(cbind(n.cars, mean.cars, sd.cars),2)
round(with(mtcars, t(sapply(mtcars[c('mpg', 'disp',  'hp')],
                            function(x) c(n=length(x), avg=mean(x),
                                          stdev=sd(x))))), 2)
library(stargazer)
stargazer(mtcars[c("mpg", "disp",  "hp")], type="text")
