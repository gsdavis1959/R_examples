library(dplyr)

df <- data.frame(team = c('A', 'A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C'),
                 points = c(12, 28, 19, 22, 32, 45, 22, 28, 13, 19),
                 rebounds = c(5, 7, 7, 12, 11, 4, 10, 7, 8, 8))
df

# rank within teams
df %>% arrange(team, points) %>%
  group_by(team) %>%
  mutate(rank = rank(points))
# descending
df %>% arrange(team, points) %>%
  group_by(team) %>%
  mutate(rank = rank(-points))