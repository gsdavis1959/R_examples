library(tidyverse)

d <- Orange

head(d)

d_ends <- d %>% 
  group_by(Tree) %>% 
  top_n(1, age) %>% 
  pull(circumference)

d_ends

ggplot(d, aes(age, circumference, color = Tree)) +
  geom_line()

ggplot(d, aes(age, circumference, color = Tree)) +
  geom_line() +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends))

ggplot(d, aes(age, circumference, color = Tree)) +
  geom_line() +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
  scale_x_continuous(expand = c(0, 0))

d <- Orange %>% 
  as_tibble()

d_ends <- d %>% 
  group_by(Tree) %>% 
  top_n(1, age) %>% 
  pull(circumference)

d %>% 
  ggplot(aes(age, circumference, color = Tree)) +
  geom_line(size = 2, alpha = .8) +
  theme_minimal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(sec.axis = sec_axis(~ ., breaks = d_ends)) +
  ggtitle("Orange trees getting bigger with age",
          subtitle = "Based on the Orange data set in R") +
  labs(x = "Days old", y = "Circumference (mm)", caption = "Plot by @drsimonj")
