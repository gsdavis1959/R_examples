if(!require(pacman))install.packages("pacman")

pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
library(tidyverse)
library(ggthemes)
# bbc theme
#Data for chart from gapminder package
line_df <- gapminder %>%
  filter(country == "Malawi") 

#Make plot
line <- ggplot(line_df, aes(x = year, y = lifeExp)) +
  geom_line(colour = "#1380A1", size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  bbc_style() +
  labs(title="Living longer",
       subtitle = "Life expectancy in Malawi 1952-2007")
line
finalise_plot(plot_name = line,
              source = "Source: Gapminder",
              # save_filepath = "images/line_plot_finalised_test.png",
              width_pixels = 640,
              height_pixels = 550)

# fiveThirtyEight theme
library(ggthemes)
p <- ggplot(mtcars, aes(x = wt, y = mpg, colour = factor(gear))) +
  geom_point() +
  facet_wrap(~am) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_fivethirtyeight() +
  theme_fivethirtyeight()
p

# Economist theme
p <- ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg, colour = factor(gear))) +
  facet_wrap(~am) +
  # Economist puts x-axis labels on the right-hand side
  scale_y_continuous(position = "right")
p
## Standard
p + theme_economist() +
  scale_colour_economist()

# Change axis lines to vertical
p + theme_economist(horizontal = FALSE) +
  scale_colour_economist() +
  coord_flip()

## White panel/light gray background
p + theme_economist_white() +
  scale_colour_economist()

## All white variant
p + theme_economist_white(gray_bg = FALSE) +
  scale_colour_economist()

## Not run: 

## The Economist uses ITC Officina Sans
library("extrafont")
p + theme_economist(base_family="ITC Officina Sans") +
  scale_colour_economist()

## Verdana is a widely available substitute
p + theme_economist(base_family="Verdana") +
  scale_colour_economist()


# example
ggplot(test1) +
  geom_line(aes(x = ref.date, y = ret.closing.prices, colour = factor(ticker))) +
  theme_economist() + theme(legend.position = "right", plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "lightblue")) +
  theme( axis.ticks.x = element_line(size = 1), axis.ticks.length=unit(0.2,"cm")) +
  scale_colour_economist() + labs(x = "Year", y = "Return on Closing Prices") +
  theme(legend.title=element_blank(), legend.text = element_text(colour="black", size = 10))



# Tufte theme
library(ggthemes)

library("ggplot2")

p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  scale_x_continuous(breaks = extended_range_breaks()(mtcars$wt)) +
  scale_y_continuous(breaks = extended_range_breaks()(mtcars$mpg)) +
  ggtitle("Cars")

p + geom_rangeframe() +
  theme_tufte()

p + geom_rug() +
  theme_tufte(ticks = FALSE)

# Tableau theme
p <- ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg, colour = factor(gear))) +
  facet_wrap(~am) +
  theme_igray()

palettes <- ggthemes_data[["tableau"]][["color-palettes"]][["regular"]]
for (palette in head(names(palettes), 3L)) {
  print(p + scale_colour_tableau(palette) +
          ggtitle(palette))
}

# WSJ theme
p <- ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg, colour = factor(gear))) +
  facet_wrap(~am) +
  ggtitle("Diamond Prices")
p + scale_colour_wsj("colors6", "") + theme_wsj()
# Use a gray background instead
p + scale_colour_wsj("colors6", "") + theme_wsj(color = "gray")

# Excel theme
p <- ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg, colour = factor(gear))) +
  facet_wrap(~am)
p + theme_excel_new() + scale_colour_excel_new()

# Titillium Web
library(hrbrthemes)
ggplot(mpg, aes(displ, hwy)) +
  geom_jitter(aes(color=class, fill=class), size=3, shape=21, alpha=1/2) +
  scale_x_continuous(expand=c(0,0), limits=c(1, 8), breaks=1:8) +
  scale_y_continuous(expand=c(0,0), limits=c(10, 50)) +
  scale_color_ipsum() +
  scale_fill_ipsum() +
  facet_wrap(~class, scales="free") +
  labs(
    title="Titillium Web",
    subtitle="This is a subtitle to see the how it looks in Titillium Web",
    caption="Source: hrbrthemes & Google"
  ) +
  theme_ipsum_tw(grid="XY", axis="xy") +
  theme(legend.position="none") -> gg

flush_ticks(gg)
theme(axis.text.x=element_text(hjust=c(0, rep(0.5, 6), 1))) +
theme(axis.text.y=element_text(vjust=c(0, rep(0.5, 3), 1)))

ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(color=factor(carb))) +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  scale_color_ipsum() +
  theme_ipsum_rc()

count(mpg, class) %>% 
  mutate(pct=n/sum(n)) %>% 
  ggplot(aes(class, pct)) +
  geom_col() +
  scale_y_percent() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 column chart example with percents",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") + 
  theme_ipsum(grid="Y")

update_geom_font_defaults(font_rc_light)

count(mpg, class) %>% 
  mutate(n=n*2000) %>% 
  arrange(n) %>% 
  mutate(class=factor(class, levels=class)) %>% 
  ggplot(aes(class, n)) +
  geom_col() +
  geom_text(aes(label=scales::comma(n)), hjust=0, nudge_y=2000) +
  scale_y_comma(limits=c(0,150000)) +
  coord_flip() +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 column chart example with commas",
       subtitle="A plot that is only useful for demonstration purposes, esp since you'd never\nreally want direct labels and axis labels",
       caption="Brought to you by the letter 'g'") + 
  theme_ipsum_rc(grid="X")

# IBM
ggplot(mpg, aes(displ, hwy)) +
  geom_jitter(aes(color=class, fill=class), size=3, shape=21, alpha=1/2) +
  scale_x_continuous(expand=c(0,0), limits=c(1, 8), breaks=1:8) +
  scale_y_continuous(expand=c(0,0), limits=c(10, 50)) +
  scale_color_ipsum() +
  scale_fill_ipsum() +
  facet_wrap(~class, scales="free") +
  labs(
    title="IBM Plex Sans Test",
    subtitle="This is a subtitle to see the how it looks in IBM Plex Sans",
    caption="Source: hrbrthemes & IBM"
  ) +
  theme_ipsum_ps(grid="XY", axis="xy") +
  theme(legend.position="none") -> gg

flush_ticks(gg)
theme(axis.text.x=element_text(hjust=c(0, rep(0.5, 6), 1))) +
theme(axis.text.y=element_text(vjust=c(0, rep(0.5, 3), 1)))


# add annotations
g <- g + annotate("text", x = 2000.5, y = 38, label = "suicides per\n 100,000 people")
g <- g + annotate("text", x = 2014, y = 43, label = "Age\nGroup", size = 3, fontface = "bold", hjust = .2)

# legend
g <- g + theme(legend.position = "none", plot.background = element_rect(fill = "white"), panel.background = element_rect(fill = "white"))

# line legends
g <- g + geom_dl(aes(label = age_group, x = year + 0.2), method = "last.qp", cex = 0.5)

# axis ticks
g <- g + theme(axis.ticks.x = element_line(size = 1), axis.ticks.length=unit(0.2,"cm"))

# add labels
p + labs(x = "New x label")
