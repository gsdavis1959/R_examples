library(tidyverse)
library(knitr) #To make the table look pretty on HTML

cefr_hours <- tibble(cefr = as_factor(c("A0", "A1", "A2", "B1", "B2", "C1", "C2")),
                     hours = c(0, 100, 200, 400, 600, 800, 1200))

kable(cefr_hours)

# expand the dataframe
cefr_hours <- tibble(cefr = as_factor(c("A0", "A1", "A2", "B1", "B2", "C1", "C2")),
                     hours = c(0, 100, 200, 400, 600, 800, 1200))

cefr_hours <- cefr_hours %>% 
  bind_rows(cefr_hours)

kable(cefr_hours)

# create groups
dplyr::mutate

cefr_hours <- tibble(cefr = as_factor(c("A0", "A1", "A2", "B1", "B2", "C1", "C2")),
                     hours = c(0, 100, 200, 400, 600, 800, 1200))

cefr_hours <- cefr_hours %>% 
  bind_rows(cefr_hours) %>% 
  arrange(cefr) %>% 
  mutate(group = ceiling((row_number() - 1) / 2)) 

kable(cefr_hours)


# arrange and rempve unncessary groups
cefr_hours <- tibble(cefr = as_factor(c("A0", "A1", "A2", "B1", "B2", "C1", "C2")),
                     hours = c(0, 100, 200, 400, 600, 800, 1200))


cefr_hours <- cefr_hours %>% 
  bind_rows(cefr_hours) %>%  
  arrange(cefr) %>% 
  mutate(group = ceiling((row_number() - 1) / 2)) %>% 
  filter(group != min(group), group != max(group))

kable(cefr_hours)

# plot
ggplot(data = cefr_hours, mapping =aes(x= cefr, y=hours, group = group, fill = group)) +
  geom_ribbon(aes(ymin = 0, ymax = hours))

ggplot(data = cefr_hours, mapping =aes(x= cefr, y=hours, group = group, fill = group)) +
  geom_ribbon(aes(ymin = 0, ymax = hours)) +
  scale_color_brewer(palette = "Blues") +
  theme_minimal() + # Set the theme
  labs(title = "Hours of Guided Learning Per Level", # Give the plot a title 
       subtitle = "Source: Cambridge English Assessment", # Give it a subtitle
       x = "", # Remove the title on the x axis
       y = "") + # Remove the title on the y axis
  theme(legend.position = "none", # Delete the legend
        axis.text.x = element_text(size = 20), # Set the size to 20
        axis.text.y = element_text(size = 20), # Set the size to 20
        plot.title = element_text(size = 25)) # Set the size to 25
