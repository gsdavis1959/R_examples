library(tidyverse)
library(lubridate)
library(pwt9)
library(brotools)
devtools::install_github('bbc/bbplot')

pwt <- pwt9.0 %>%
  select(country, year, avh)
head(pwt)

pwt_wide <- pwt %>%
  pivot_wider(names_from = year, values_from = avh)  %>%
  filter(!is.na(`1950`)) %>%
  mutate_at(vars(-country), as.numeric)

wss <- map_dbl(1:5, ~{kmeans(select(pwt_wide, -country), ., nstart=50,iter.max = 15 )$tot.withinss})

n_clust <- 1:5

elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))

library(ggthemes)
ggplot(elbow_df) +
  geom_line(aes(y = wss, x = n_clust), colour = "#82518c") +
  theme_minimal()

clusters <- kmeans(select(pwt_wide, -country), centers = 3)
(centers <- rownames_to_column(as.data.frame(clusters$centers), "cluster"))

pwt_wide <- pwt_wide %>% 
  mutate(cluster = clusters$cluster)

pwt_long <- pwt_wide %>%
  pivot_longer(cols=c(-country, -cluster), names_to = "year", values_to = "avh") %>%
  mutate(year = ymd(paste0(year, "-01-01")))

centers_long <- centers %>%
  pivot_longer(cols = -cluster, names_to = "year", values_to = "avh") %>%  
  mutate(year = ymd(paste0(year, "-01-01")))

library(bbplot)
ggplot() +
  geom_line(data = pwt_long, aes(y = avh, x = year, group = country), colour = "#82518c") +
  facet_wrap(~cluster, nrow = 1) + 
  geom_line(data = centers_long, aes(y = avh, x = year, group = cluster), col = "#b58900", size = 2) +
  theme_economist() +
  labs(title = "Average hours worked in several countries", 
       caption = "The different time series have been clustered using k-means.
                 Cluster 1: Belgium, Switzerland, Germany, Denmark, France, Luxembourg, Netherlands,
                 Norway, Sweden.\nCluster 2: Australia, Colombia, Ireland, Iceland, Japan, Mexico,
                 Portugal, Turkey.\nCluster 3: Argentina, Austria, Brazil, Canada, Cyprus, Spain, Finland,
                 UK, Italy, New Zealand, Peru, USA, Venezuela") +
  theme(plot.caption = element_text(colour = "white"))
