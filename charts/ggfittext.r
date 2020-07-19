library(ggfittext)
library(tidyverse)
library(plotly)
data("animals")

ggplot(animals, aes(x = type, y = flies, label = animal)) +
  geom_tile(fill = "white", colour = "black") +
  geom_fit_text(reflow = TRUE)

ggplot(animals, aes(x = type, y = flies, label = animal)) +
  geom_tile(fill = "white", colour = "black") +
  geom_fit_text(place = "topleft", reflow = TRUE)

ggplot(altitudes, aes(x = craft, y = altitude, label = altitude)) +
  geom_col() +
  geom_bar_text()

ggplot(coffees, aes(x = coffee, y = proportion, label = ingredient,
                    fill = ingredient)) +
  geom_col(position = "stack") +
  geom_bar_text(position = "stack", grow = TRUE, reflow = TRUE)

ggplot(coffees, aes(x = coffee, y = proportion, label = ingredient,
                    fill = ingredient)) +
  geom_col(position = "dodge") +
  geom_bar_text(position = "dodge", grow = TRUE, reflow = TRUE, 
                place = "left") +
  coord_flip()

ggplot(gold, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                 fill = line, label = label)) +
  coord_polar() +
  geom_rect() +
  geom_fit_text(min.size = 0, grow = TRUE) +
  scale_fill_gradient(low = "#fee391", high = "#238443")

ggplot(animals, aes(x = type, y = flies, fill = mass, label = animal)) +
  geom_tile() +
  geom_fit_text(reflow = TRUE, grow = TRUE, contrast = TRUE)
