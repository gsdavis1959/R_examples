d <- mtcars
d$hp[3] <- NA
head(d)

# check multicolinearity
fit_1 <- lm(mpg ~ hp,        data = d)
fit_2 <- lm(mpg ~ hp + disp, data = d)

summary(fit_1)
summary(fit_2)

# missing values in the correlations
rs <- cor(d)
rs
# handle the missing variables in hp
rs <- cor(d, use = "pairwise.complete.obs")
rs
# select specific variables
vars <- c("mpg", "hp", "disp")
rs[rownames(rs) %in% vars, colnames(rs) %in% vars]
# correlations with the input variables (multicolinearity) - do a factor analysis
factanal(na.omit(d), factors = 2)
factanal(na.omit(d), factors = 5)
# find the variables with correlations over .9, but need to take out the diagonal
diag(rs) <- NA
col_has_over_90 <- apply(rs, 2, function(x) any(x > .9, na.rm = TRUE))
rs[, col_has_over_90]

library(tidyverse)
d %>% 
  select(mpg:drat) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free")

library(corrr)
d %>% 
  correlate() %>% 
  focus(mpg:drat, mirror = TRUE) %>% 
  network_plot()
rs <- correlate(d)
rs
correlate(d, method = "spearman", diagonal = 1)

# plot
rs %>% 
  select(mpg:drat) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key)
# over .9
any_over_90 <- function(x) any(x > .9, na.rm = TRUE)
rs %>% select_if(any_over_90)
# focus on specific columns
rs %>% 
  focus(mpg, disp, hp)
rs %>% 
  focus(-mpg, -disp, -hp)
rs %>% 
  focus(mpg, disp, hp, mirror = TRUE)
rs %>% 
  focus(matches("^d"))
rs %>% 
  focus_if(any_over_90, mirror = TRUE)
rs %>% 
  focus(mpg)
# plot one against the others
rs %>%
  focus(mpg) %>%
  mutate(rowname = reorder(rowname, mpg)) %>%
  ggplot(aes(rowname, mpg)) +
  geom_col() + coord_flip()

rs %>% rearrange()
rs %>% shave()
rs %>% stretch()

rs %>%
  shave() %>% 
  stretch(na.rm = FALSE) %>% 
  ggplot(aes(r)) +
  geom_histogram()
rs %>%
  focus(mpg:drat, mirror = TRUE) %>% 
  rearrange() %>% 
  shave(upper = FALSE) %>% 
  select(-hp) %>% 
  filter(rowname != "drat")
rs %>% fashion()

rs %>%
  focus(mpg:drat, mirror = TRUE) %>% 
  rearrange() %>% 
  shave(upper = FALSE) %>% 
  select(-hp) %>% 
  filter(rowname != "drat") %>% 
  fashion()

rs %>% rplot()
rs %>%
  rearrange(method = "MDS", absolute = FALSE) %>%
  shave() %>% 
  rplot(shape = 15, colors = c("red", "green"))
rs %>% network_plot(min_cor = .6)
