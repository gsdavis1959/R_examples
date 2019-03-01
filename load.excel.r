setwd('~/Data/Datasets')
# Please note that loading xlsx in R is really slow compared to csv
library(xlsx)
population_xls <- read.xlsx("indicator gapminder population.xlsx", encoding = "UTF-8",stringsAsFactors= F, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)
fertility_xls <- read.xlsx("indicator undata total_fertility.xlsx", encoding = "UTF-8",stringsAsFactors= F, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)
lifeexp_xls <- read.xlsx("indicator life_expectancy_at_birth.xlsx", encoding = "UTF-8", stringsAsFactors= F, sheetIndex = 1, as.data.frame = TRUE, header=TRUE)


# Load libraries
library(reshape)
library(gapminder)
library(dplyr)
library(ggplot2)
# Create a variable to keep only years 1962 to 2015
myvars <- paste("X", 1962:2015, sep="")
# Create 3 data frame with only years 1962 to 2015
population <- population_xls[c('Total.population',myvars)]
fertility <- fertility_xls[c('Total.fertility.rate',myvars)]
lifeexp <- lifeexp_xls[c('Life.expectancy',myvars)]
# Rename the first column as "Country"
colnames(population)[1] <- "Country"
colnames(fertility)[1] <- "Country"
colnames(lifeexp)[1] <- "Country"
# Remove empty lines that were created keeping only 275 countries
lifeexp <- lifeexp[1:275,]
population <- population[1:275,]
# Use reshape library to move the year dimension as a column
population_m <- melt(population, id=c("Country")) 
lifeexp_m <- melt(lifeexp, id=c("Country")) 
fertility_m <- melt(fertility, id=c("Country"))
# Give a different name to each KPI (e.g. pop, life, fert)
colnames(population_m)[3] <- "pop"
colnames(lifeexp_m)[3] <- "life"
colnames(fertility_m)[3] <- "fert"
# Merge the 3 data frames into one
mydf <- merge(lifeexp_m, fertility_m, by=c("Country","variable"), header =T)
mydf <- merge(mydf, population_m, by=c("Country","variable"), header =T)
# The only piece of the puzzle missing is the continent name for each country for the color - use gapminder library to bring it
continent <- gapminder %>% group_by(continent, country) %>% distinct(country, continent)
continent <- data.frame(lapply(continent, as.character), stringsAsFactors=FALSE)
colnames(continent)[1] <- "Country"
# Filter out all countries that do not exist in the continent table
mydf_filter <- mydf %>% filter(Country %in% unique(continent$Country))
# Add the continent column to finalize the data set
mydf_filter <- merge(mydf_filter, continent, by=c("Country"), header =T)
# Do some extra cleaning (e.g. remove N/A lines, remove factors, and convert KPIs into numerical values)
mydf_filter[is.na(mydf_filter)] <- 0
mydf_filter <- data.frame(lapply(mydf_filter, as.character), stringsAsFactors=FALSE)
mydf_filter$variable <- as.integer(as.character(gsub("X","",mydf_filter$variable)))
colnames(mydf_filter)[colnames(mydf_filter)=="variable"] <- "year"
mydf_filter$pop <- round(as.numeric(as.character(mydf_filter$pop))/1000000,1)
mydf_filter$fert <- as.numeric(as.character(mydf_filter$fert))
mydf_filter$life <- as.numeric(as.character(mydf_filter$life))

# Load libraries
library(ggplot2)
library(gganimate)
#library(gifski)
#library(png)
# Add a global theme
theme_set(theme_grey()+ theme(legend.box.background = element_rect(),legend.box.margin = margin(6, 6, 6, 6)) )
# OLD VERSION
# Create the plot with years as frame, limiting y axis from 30 years to 100
# p <- ggplot(mydf_filter, aes(fert, life, size = pop, color = continent, frame = variable)) +
#  geom_point()+ ylim(30,100)  + labs(x="Fertility Rate", y = "Life expectancy at birth (years)", caption = "(Based on data from Hans Rosling - gapminder.com)", color = 'Continent',size = "Population (millions)") + 
#  scale_color_brewer(type = 'div', palette = 'Spectral') 
# gganimate(p, interval = .2, "output.gif")
# NEW VERSION
# Create the plot with years as frame, limiting y axis from 30 years to 100
p <- ggplot(mydf_filter, aes(fert, life, size = pop, color = continent, frame = year)) +
  labs(x="Fertility Rate", y = "Life expectancy at birth (years)", caption = "(Based on data from Hans Rosling - gapminder.com)", color = 'Continent',size = "Population (millions)") + 
  ylim(30,100) +
  geom_point() +
  scale_color_brewer(type = 'div', palette = 'Spectral') + 
  # gganimate code
  ggtitle("Year: {frame_time}") +
  transition_time(year) +
  ease_aes("linear") +
  enter_fade() +
  exit_fade()
# animate
animate(p, width = 450, height = 450)
# save as a GIF
anim_save("world.gif")

# Load libraries
library(plotly)
library(ggplot2)
# Create the plot
p <- ggplot(mydf_filter, aes(fert, life, size = pop, color = continent, frame = year)) +
  geom_point()+ ylim(30,100)  + labs(x="Fertility Rate", y = "Life expectancy at birth (years)", color = 'Continent',size = "Population (millions)") + 
  scale_color_brewer(type = 'div', palette = 'Spectral')


# Generate the Visual and a HTML output
ggp <- ggplotly(p, height = 900, width = 900) %>%
  animation_opts(frame = 100,
                 easing = "linear",
                 redraw = FALSE)
ggp
htmlwidgets::saveWidget(ggp, "index.html")
